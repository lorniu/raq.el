;;; raq.el --- HTTP Library Adapter, support url, plz and more -*- lexical-binding: t -*-

;; Copyright (C) 2025 lorniu <lorniu@gmail.com>

;; Author: lorniu <lorniu@gmail.com>
;; URL: https://github.com/lorniu/raq.el
;; License: GPL-3.0-or-later
;; Package-Requires: ((emacs "29.1"))
;; Version: 0.1

;; This file is not part of GNU Emacs.

;; This program is free software: you can redistribute it and/or modify
;; it under the terms of the GNU General Public License as published by
;; the Free Software Foundation, either version 3 of the License, or
;; (at your option) any later version.

;; This program is distributed in the hope that it will be useful,
;; but WITHOUT ANY WARRANTY; without even the implied warranty of
;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
;; GNU General Public License for more details.

;; You should have received a copy of the GNU General Public License
;; along with this program.  If not, see <http://www.gnu.org/licenses/>.

;;; Commentary:
;;
;; HTTP Library Adapter, support url.el and plz.el, and can be extended.
;;
;;  - API is simple and uniform
;;  - Support both sync/async request
;;  - Support streaming request
;;  - Support retry for timeout
;;  - Support config proxies for client
;;  - Support file upload/download
;;
;; See README.md of https://github.com/lorniu/raq.el for more details.

;;; Code:

(require 'cl-lib)
(require 'url)
(require 'eieio)

(defgroup raq nil
  "HTTP Library Adapter."
  :group 'network
  :prefix 'raq-)

(defcustom raq-debug nil
  "Debug flag."
  :type 'boolean)

(defcustom raq-user-agent "Mozilla/5.0 (X11; Linux x86_64) AppleWebKit/537.36 (KHTML, like Gecko) Chrome/86.0.4240.75 Safari/537.36"
  "Default user agent used by request."
  :type 'string)

(defcustom raq-max-retry 3
  "Default retry times when request timeout."
  :type 'integer)

(defcustom raq-multipart-boundary "raq-boundary-=-=+O0o0O69Oo"
  "A string used as multipart boundary."
  :type 'string)

(defun raq-log (fmt &rest args)
  "Output log when `raq-debug' not nil.
FMT and ARGS are like arguments in `message'."
  (when raq-debug
    (apply #'message (format "[raq] %s" fmt) args)))

(defun raq-http-binary-p (content-type)
  "Check if current CONTENT-TYPE is binary."
  (if (null content-type) nil
    (cl-destructuring-bind (mime sub) (string-split content-type "/" nil "[ \n\r\t]")
      (not (or (equal mime "text")
               (and (equal mime "application")
                    (string-match-p "json\\|xml\\|php" sub)))))))

(defun raq-format-params (alist)
  "Format ALIST to k=v style query string."
  (mapconcat (lambda (arg)
               (format "%s=%s"
                       (url-hexify-string (format "%s" (car arg)))
                       (url-hexify-string (format "%s" (or (cdr arg) 1)))))
             (delq nil alist) "&"))

(defun raq-format-formdata (alist)
  "Generate multipart/formdata string from ALIST."
  (with-temp-buffer
    (set-buffer-multibyte nil)
    (cl-loop for (key . value) in alist for i from 1
             for filep = nil for contentype = nil
             do (setq key (format "%s" key))
             do (if (consp value) ; ((afile "~/aaa.jpg" "image/jpeg"))
                    (setq contentype (or (cadr value) "application/octet-stream")
                          value (format "%s" (car value)) filep t)
                  (setq value (format "%s" value)))
             for newline = "\r\n"
             do (insert "--" raq-multipart-boundary newline)
             if filep do (let ((fn (url-encode-url (url-file-nondirectory value))))
                           (insert "Content-Disposition: form-data; name=\"" key "\" filename=\"" fn "\"" newline)
                           (insert "Content-Type: " contentype newline newline)
                           (insert-file-contents-literally value)
                           (goto-char (point-max)))
             else do (insert "Content-Disposition: form-data; name=\"" key "\"" newline newline value)
             if (< i (length alist)) do (insert newline)
             else do (insert newline "--" raq-multipart-boundary "--"))
    (buffer-substring-no-properties (point-min) (point-max))))


;;; Core

(defvar-local raq-stream-abort-flag nil
  "Non-nil means to ignore following stream progress in callback of http filter.")

(defclass raq-client ()
  ((insts :allocation :class :initform nil)
   (user-agent :initarg :user-agent :initform nil :type (or string null)))
  "Used to send http request."
  :abstract t)

(cl-defmethod make-instance ((class (subclass raq-client)) &rest slots)
  "Ensure CLASS with same SLOTS only has one instance."
  (if-let* ((key (sha1 (format "%s" slots)))
            (insts (oref-default class insts))
            (old (cdr-safe (assoc key insts))))
      old
    (let ((inst (cl-call-next-method)))
      (prog1 inst (oset-default class insts `((,key . ,inst) ,@insts))))))

(cl-defgeneric raq (http-client url &rest _args &key method headers data filter done fail sync retry &allow-other-keys)
  "Send HTTP request using the given HTTP-CLIENT.

Keyword arguments:
  - URL: The URL to send the request to.
  - METHOD: Request method, symbol like \\='post.  If nil guess by data.
  - HEADERS: Additional headers to include in the request.  Alist.
  - DATA: The data to include in the request.  If this is a string, it will be
          sent directly as request body.  If this is a list and every element
          is (key . value) then this will be joined to a string like a=1&b=2 and
          then be sent.  If this is a list and some element is (key filename)
          format, then the list will be normalized as multipart formdata string
          and be sent.
  - FILTER: A function to be called every time when some data returned.
  - DONE: A function to be called when the request succeeds.
  - FAIL: A function to be called when the request fails.
  - RETRY: How many times it can retry for timeout.  Number.
  - SYNC: Non-nil means request synchronized.  Boolean.

If request async, return the process behind the request."
  (:method :around ((client raq-client) url &rest args &key method _headers data filter done fail sync retry)
           ;; normalize and validate
           (if (and (null filter) (null done)) (setq sync t args `(:sync t ,@args)))
           (cl-assert (and url (or (and sync (not filter)) (and (not sync) (or filter done)))))
           (if (null method) (setq args `(:method ,(if data 'post 'get) ,@args)))
           ;; sync
           (if sync (apply #'cl-call-next-method client url args)
             ;; async
             (let* ((failfn (lambda (status)
                              ;; retry for timeout
                              (unless retry (setq retry raq-max-retry))
                              (if (and (string-match-p "Operation timeout" (format "%s" status)) (cl-plusp retry))
                                  (progn (raq-log "Request timeout, retrying (remains %d times)..." retry)
                                         (apply #'raq client url `(:retry ,(1- retry) ,@args)))
                                ;; failed finally
                                (raq-log "REQUEST FAILED: (%s) %s" url status)
                                (if fail (funcall fail status)
                                  (signal (car status) (cdr status))))))
                    (filterfn (when filter
                                (lambda ()
                                  ;; abort action and error case
                                  (unless raq-stream-abort-flag
                                    (condition-case err
                                        (funcall filter)
                                      (error
                                       (setq raq-stream-abort-flag t)
                                       (raq-log "Error in filter: (%s) %s" url err)
                                       (funcall failfn err)))))))
                    (donefn (lambda (raw)
                              (raq-log "✓ %s" url)
                              (when done (funcall done raw)))))
               (apply #'cl-call-next-method client url `(:fail ,failfn :filter ,filterfn :done ,donefn ,@args))))))


;;; Implement of url.el

(defclass raq-url-client (raq-client)
  ((proxy-services
    :initarg :proxies
    :initform nil
    :type (or list null)
    :documentation "Proxy services passed to `url.el', see `url-proxy-services' for details."))
  :documentation "Http Client implemented using `url.el'.")

(defvar url-http-content-type)
(defvar url-http-end-of-headers)
(defvar url-http-transfer-encoding)

(defvar raq-url-extra-filter nil)

(defun raq-url-http-extra-filter (beg end len)
  "Call `raq-url-extra-filter'.  BEG, END and LEN see `after-change-functions'."
  (when (and raq-url-extra-filter (bound-and-true-p url-http-end-of-headers)
             (if (equal url-http-transfer-encoding "chunked") (= beg end) ; when delete
               (= len 0))) ; when insert
    (save-excursion
      (save-restriction
        (narrow-to-region url-http-end-of-headers (point-max))
        (funcall raq-url-extra-filter)))))

(cl-defmethod raq ((client raq-url-client) url &key method headers data filter done fail sync retry)
  "Send a request with CLIENT.
See the generic method for args URL, METHOD, HEADERS, DATA, FILTER, DONE, FAIL,
SYNC and RETRY and more."
  (ignore retry)
  (let* ((inhibit-message t)
         (message-log-max nil)
         (url-user-agent (or (oref client user-agent) raq-user-agent))
         (url-proxy-services (or (oref client proxy-services) url-proxy-services))
         (formdatap (and (consp data)
                         (or (string-match-p "multipart/formdata"
                                             (or (alist-get "Content-Type" headers nil nil #'string-equal-ignore-case) ""))
                             (cl-some (lambda (x) (consp (cdr x))) data))))
         (url-request-data (funcall (if (atom data) #'identity ; string
                                      (if formdatap #'raq-format-formdata #'raq-format-params)) ; alist
                                    data))
         (url-request-extra-headers (progn
                                      (when formdatap
                                        (setf (alist-get "Content-Type" headers nil nil #'string-equal-ignore-case)
                                              (concat "multipart/form-data; boundary=" raq-multipart-boundary)))
                                      headers))
         (url-request-method (string-to-unibyte (upcase (format "%s" method))))
         (url-mime-encoding-string "identity")
         (get-resp-content (lambda ()
                             (set-buffer-multibyte (not (raq-http-binary-p url-http-content-type)))
                             (buffer-substring-no-properties (min (1+ url-http-end-of-headers) (point-max)) (point-max)))))
    ;; log
    (raq-log "> %s" url)
    (if url-request-extra-headers (raq-log "> HEADER: %S" url-request-extra-headers))
    (if url-request-data (raq-log "> DATA:   %s" url-request-data))
    ;; sync
    (if sync
        (condition-case err
            (let ((buf (url-retrieve-synchronously url nil t)))
              (unwind-protect
                  (with-current-buffer buf
                    (let ((s (funcall get-resp-content))) (if done (funcall done s) s)))
                (ignore-errors (kill-buffer buf))))
          (error (if fail (funcall fail err) (signal 'user-error (cdr err)))))
      ;; async
      (let ((buf (url-retrieve url
                               (lambda (status)
                                 (let ((cb (current-buffer)))
                                   (remove-hook 'after-change-functions #'raq-url-http-extra-filter t)
                                   (unwind-protect
                                       (if-let* ((err (or (cdr-safe (plist-get status :error))
                                                          (when (or (null url-http-end-of-headers) (= 1 (point-max)))
                                                            (list 'empty-response "Nothing response from server")))))
                                           (if fail (funcall fail err) (signal 'user-error err))
                                         (if done (funcall done (funcall get-resp-content))))
                                     (kill-buffer cb))))
                               nil t)))
        (when (and filter (buffer-live-p buf))
          (with-current-buffer buf
            (setq-local raq-url-extra-filter filter)
            (add-hook 'after-change-functions #'raq-url-http-extra-filter nil t)))
        (get-buffer-process buf)))))


;;; Implement of plz.el

(defclass raq-plz-client (raq-client)
  ((extra-args
    :initarg :args
    :type list
    :documentation "Extra arguments passed to curl program."))
  :documentation "Http Client implemented using `plz.el'.")

(defvar plz-curl-program)
(defvar plz-curl-default-args)
(defvar plz-http-end-of-headers-regexp)

(declare-function plz "ext:plz.el" t t)
(declare-function plz-error-message "ext:plz.el" t t)
(declare-function plz-error-curl-error "ext:plz.el" t t)
(declare-function plz-error-response "ext:plz.el" t t)
(declare-function plz-response-status "ext:plz.el" t t)
(declare-function plz-response-body "ext:plz.el" t t)
(declare-function plz--narrow-to-body "ext:plz.el" t t)

(defvar raq-plz-initialize-error-message
  "\n\nTry to install curl and specify the program like this to solve the problem:\n
  (setq plz-curl-program \"c:/msys64/usr/bin/curl.exe\")\n
Or switch http client to `raq-url-http-client' instead:\n
  (setq raq-default-http-client (raq-url-http-client))")

(cl-defmethod raq :before ((_ raq-plz-client) &rest _)
  "Check if `plz.el' is available."
  (unless (and (require 'plz nil t) (executable-find plz-curl-program))
    (error "You should have `plz.el' and `curl' installed before using `raq-plz'")))

(cl-defmethod raq ((client raq-plz-client) url &key method headers data filter done fail sync retry)
  "Send a request with CLIENT.
See the generic method for args URL, METHOD, HEADERS, DATA, FILTER, DONE, FAIL,
SYNC and RETRY and more."
  (ignore retry)
  (let* ((plz-curl-default-args (if (slot-boundp client 'extra-args)
                                    (append (oref client extra-args) plz-curl-default-args)
                                  plz-curl-default-args))
         (formdatap (and (consp data)
                         (or (string-match-p "multipart/formdata"
                                             (or (alist-get "Content-Type" headers nil nil #'string-equal-ignore-case) ""))
                             (cl-some (lambda (x) (consp (cdr x))) data))))
         (data (funcall (if (atom data) #'identity ; string
                          (if formdatap #'raq-format-formdata #'raq-format-params)) ; alist
                        data))
         (string-or-binary (lambda () ; decode according content-type. there is no builtin way to do this in plz
                             (widen)
                             (let* ((content-type (mail-fetch-field "content-type"))
                                    (binaryp (raq-http-binary-p content-type)))
                               (set-buffer-multibyte (not binaryp))
                               (goto-char (point-min))
                               (plz--narrow-to-body)
                               (unless binaryp (decode-coding-region (point-min) (point-max) 'utf-8))
                               (buffer-string)))))
    ;; headers
    (when formdatap
      (setf (alist-get "Content-Type" headers nil nil #'string-equal-ignore-case)
            (concat "multipart/form-data; boundary=" raq-multipart-boundary)))
    (unless (alist-get "User-Agent" headers nil nil #'string-equal-ignore-case)
      (push `("User-Agent" . ,(or (oref client user-agent) raq-user-agent)) headers))
    ;; log
    (raq-log "> %s" url)
    (if headers (raq-log "> HEADER: %s" headers))
    (if data (raq-log "> DATA:   %s" data))
    (if plz-curl-default-args (raq-log "> EXTRA: %s" plz-curl-default-args))
    ;; sync
    (if sync
        (condition-case err
            (let ((r (plz method url
                       :headers headers
                       :body data
                       :body-type (if formdatap 'binary 'text)
                       :decode nil
                       :as string-or-binary
                       :then 'sync)))
              (if done (funcall done r) r))
          (error (if fail (funcall fail err)
                   (signal 'user-error (cdr err)))))
      ;; async
      (plz method url
        :headers headers
        :body data
        :body-type (if formdatap 'binary 'text)
        :decode nil
        :as string-or-binary
        :filter (when filter
                  (lambda (proc string)
                    (with-current-buffer (process-buffer proc)
                      (save-excursion
                        (goto-char (point-max))
                        (insert string)
                        (goto-char (point-min))
                        (when (re-search-forward plz-http-end-of-headers-regexp nil t)
                          (save-restriction
                            (narrow-to-region (point) (point-max))
                            (funcall filter)))))))
        :then (lambda (raw)
                (when done (funcall done raw)))
        :else (lambda (err)
                (let ((ret ;; try to compatible with error object of url.el, see `url-retrieve' for details
                       (or (plz-error-message err)
                           (when-let* ((r (plz-error-curl-error err)))
                             (list 'curl-error
                                   (concat (format "%s" (or (cdr r) (car r)))
                                           (pcase (car r)
                                             (2 (when (memq system-type '(cygwin windows-nt ms-dos))
                                                  raq-plz-initialize-error-message))))))
                           (when-let* ((r (plz-error-response err)))
                             (list 'http (plz-response-status r) (plz-response-body r))))))
                  (if fail (funcall fail ret)
                    (signal 'user-error ret))))))))



(defvar raq-default-client
  (if (and (require 'plz nil t) (executable-find plz-curl-program))
      (raq-plz-client)
    (raq-url-client))
  "Client used by `raq' by default.
This should be instance of symbol `raq-client', or a function with current
host as argument that return an instance.  If is a function, the client be
used will be determined dynamically when the `raq' be called.")

;;;###autoload
(cl-defmethod raq (&rest args)
  "Send a request with `raq-default-client'.
In this case, the first argument in ARGS should be url instead of client.
See the generic method for other ARGS and details."
  (let ((client (if (functionp raq-default-client)
                    (funcall raq-default-client
                             (url-host (url-generic-parse-url (car args))))
                  raq-default-client)))
    (unless (and client (eieio-object-p client) (object-of-class-p client 'raq-client))
      (user-error "Make sure `raq-default-client' is available.  eg:\n\n(setq raq-default (raq-url))\n\n\n"))
    (apply #'raq client args)))

(provide 'raq)

;;; raq.el ends here
