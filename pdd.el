;;; pdd.el --- HTTP Library Adapter, support url, plz and more -*- lexical-binding: t -*-

;; Copyright (C) 2025 lorniu <lorniu@gmail.com>

;; Author: lorniu <lorniu@gmail.com>
;; URL: https://github.com/lorniu/pdd.el
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
;; See README.md of https://github.com/lorniu/pdd.el for more details.

;;; Code:

(require 'cl-lib)
(require 'url)
(require 'eieio)
(require 'help)

(defgroup pdd nil
  "HTTP Library Adapter."
  :group 'network
  :prefix 'pdd-)

(defcustom pdd-debug nil
  "Debug flag."
  :type 'boolean)

(defcustom pdd-user-agent "Mozilla/5.0 (X11; Linux x86_64) AppleWebKit/537.36 (KHTML, like Gecko) Chrome/86.0.4240.75 Safari/537.36"
  "Default user agent used by request."
  :type 'string)

(defcustom pdd-max-retry 1
  "Default retry times when request timeout."
  :type 'integer)

(defcustom pdd-multipart-boundary "pdd-boundary-=-=+O0o0O69Oo"
  "A string used as multipart boundary."
  :type 'string)

(defun pdd-log (tag fmt &rest args)
  "Output log to *Messages* buffer.
TAG usually is the name of current http client.
FMT and ARGS are arguments same as function `message'."
  (apply #'message (format "[%s] %s" (or tag "pdd") fmt) args))

(defun pdd-binary-type-p (content-type)
  "Check if current CONTENT-TYPE is binary."
  (when content-type
    (cl-destructuring-bind (mime sub) (string-split content-type "/" nil "[ \n\r\t]")
      (not (or (equal mime "text")
               (and (equal mime "application")
                    (string-match-p "json\\|xml\\|php" sub)))))))

(defun pdd-format-params (alist)
  "Format ALIST to k=v style query string."
  (mapconcat (lambda (arg)
               (format "%s=%s"
                       (url-hexify-string (format "%s" (car arg)))
                       (url-hexify-string (format "%s" (or (cdr arg) 1)))))
             (delq nil alist) "&"))

(defun pdd-format-formdata (alist)
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
             do (insert "--" pdd-multipart-boundary newline)
             if filep do (let ((fn (url-encode-url (url-file-nondirectory value))))
                           (insert "Content-Disposition: form-data; name=\"" key "\" filename=\"" fn "\"" newline)
                           (insert "Content-Type: " contentype newline newline)
                           (insert-file-contents-literally value)
                           (goto-char (point-max)))
             else do (insert "Content-Disposition: form-data; name=\"" key "\"" newline newline value)
             if (< i (length alist)) do (insert newline)
             else do (insert newline "--" pdd-multipart-boundary "--"))
    (buffer-substring-no-properties (point-min) (point-max))))

(defun pdd-extract-http-headers ()
  "Extract http headers from the current responsed buffer."
  (save-excursion
    (goto-char (point-min))
    (forward-line 1)
    (mapcar (lambda (elt)
              (cons (car elt) (string-trim (cdr elt))))
            (mail-header-extract))))

(defun pdd-funcall (fn args)
  "Funcall FN and pass some of ARGS to it according its arity."
  (let ((n (car (func-arity fn))))
    (apply fn (cl-loop for i from 1 to n for x in args collect x))))


;;; Core

(defvar-local pdd-stream-abort-flag nil
  "Non-nil means to ignore following stream progress in callback of http filter.")

(defvar pdd-default-error-handler nil
  "The default error handler which is a function with current error as argument.
When error occurrs and no :fail specified, this will perform as the handler.
Besides globally set, it also can be dynamically binding in let.")

(defvar pdd-default-config nil)

;; config
(defclass pdd-config ()
  ((url)
   (method)
   (params)
   (headers)
   (data)
   (base-url)
   (timeout)
   (done)
   (fail)
   (finally)
   (request-interceptors)
   (response-interceptors)
   (param-serializer) ; param => string
   (request-transformer) ; data, header => data, header
   (response-transformer)
   (timer)
   (queue)) ; string => data
  )

(defun pdd-all (lst &key done fail))

'(pdd-all
 (mapcar
  (lambda (name) (pdd (format "https://api.github.com/users/%s" name) :done #'identity))
  '("jeresig" "iliakan" "remy"))
 :done (lambda (lst) (cl-loop for i in lst do (message i)))
 :fail (lambda (err) (message "error")))

(defun pdd-any () )
(defun pdd-race () )

(defclass pdd-client ()
  ((insts :allocation :class :initform nil)
   (user-agent :initarg :user-agent :initform nil :type (or string null))
   (config))
  "Used to send http request."
  :abstract t)

(cl-defmethod make-instance ((class (subclass pdd-client)) &rest slots)
  "Ensure CLASS with same SLOTS only has one instance."
  (if-let* ((key (sha1 (format "%s" slots)))
            (insts (oref-default class insts))
            (old (cdr-safe (assoc key insts))))
      old
    (let ((inst (cl-call-next-method)))
      (prog1 inst (oset-default class insts `((,key . ,inst) ,@insts))))))

(cl-defgeneric pdd (pdd-client url &rest _args &key method headers data filter done fail always sync retry &allow-other-keys)
  "Send HTTP request using the given PDD-CLIENT.

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
  - ALWAYS: A function to be called whether done or fail.
  - RETRY: How many times it can retry for timeout.  Number.
  - SYNC: Non-nil means request synchronized.  Boolean.

If request async, return the process behind the request."
  (:method :around ((client pdd-client) url &rest args &key method _headers data filter done fail sync retry)
           ;; keyword :done can be ignored
           (when (functionp (car args))
             (push :done args)
             (setq done (car args)))
           ;; normalize and validate
           (if (and (null filter) (null done)) (setq sync t args `(:sync t ,@args)))
           (cl-assert (and url (or (and sync (not filter)) (and (not sync) (or filter done)))))
           (if (null method) (setq args `(:method ,(if data 'post 'get) ,@args)))
           ;; sync
           (if sync (apply #'cl-call-next-method client url args)
             ;; async
             (let* ((tag (eieio-object-class client))
                    (buf (current-buffer))
                    (handler pdd-default-error-handler)
                    (failfn (lambda (status)
                              ;; retry for timeout
                              (unless retry (setq retry pdd-max-retry))
                              (if (and (string-match-p "peration timeout" (format "%s" status)) (cl-plusp retry))
                                  (progn
                                    (let ((inhibit-message t))
                                      (message "Timeout, retrying (%d)..." retry))
                                    (if pdd-debug (pdd-log tag "Request timeout, retrying (remains %d times)..." retry))
                                    (apply #'pdd client url `(:retry ,(1- retry) ,@args)))
                                ;; failed finally
                                (if pdd-debug (pdd-log tag "REQUEST FAILED: (%s) %s" url status))
                                (with-current-buffer (if (buffer-live-p buf) buf (current-buffer))
                                  (if fail (funcall fail status)
                                    (if handler (funcall handler status)
                                      (print status)))))))
                    (filterfn (when filter
                                (lambda ()
                                  ;; abort action and error case
                                  (unless pdd-stream-abort-flag
                                    (condition-case err
                                        (funcall filter)
                                      (error
                                       (setq pdd-stream-abort-flag t)
                                       (if pdd-debug (pdd-log tag "Error in filter: (%s) %s" url err))
                                       (funcall failfn err)))))))
                    (arglst (cl-loop for arg in (if (equal (func-arity done) '(0 . many)) '(a1)
                                                  (help-function-arglist done))
                                     until (memq arg '(&rest &optional &key))
                                     collect arg))
                    (donefn (if (> (length arglst) 4)
                                (user-error "Function :done has invalid arguments")
                              `(lambda ,arglst
                                 (if pdd-debug (pdd-log ,tag "Done!"))
                                 (with-current-buffer (if (buffer-live-p ,buf) ,buf (current-buffer))
                                   (,done ,@arglst))))))
               (apply #'cl-call-next-method client url `(:fail ,failfn :filter ,filterfn :done ,donefn ,@args)))))
  (declare (indent 1)))


;;; Implement of url.el

(defclass pdd-url-client (pdd-client)
  ((proxy-services
    :initarg :proxies
    :initform nil
    :type (or list null)
    :documentation "Proxy services passed to `url.el', see `url-proxy-services' for details."))
  :documentation "Http Client implemented using `url.el'.")

(defvar url-http-content-type)
(defvar url-http-end-of-headers)
(defvar url-http-transfer-encoding)
(defvar url-http-response-status)
(defvar url-http-response-version)

(defvar pdd-url-extra-filter nil)

(defun pdd-url-http-extra-filter (beg end len)
  "Call `pdd-url-extra-filter'.  BEG, END and LEN see `after-change-functions'."
  (when (and pdd-url-extra-filter (bound-and-true-p url-http-end-of-headers)
             (if (equal url-http-transfer-encoding "chunked") (= beg end) ; when delete
               (= len 0))) ; when insert
    (save-excursion
      (save-restriction
        (narrow-to-region url-http-end-of-headers (point-max))
        (funcall pdd-url-extra-filter)))))

(cl-defmethod pdd ((client pdd-url-client) url &key method headers data filter done fail sync retry)
  "Send a request with CLIENT.
See the generic method for args URL, METHOD, HEADERS, DATA, FILTER, DONE, FAIL,
SYNC and RETRY and more."
  (ignore retry)
  (let* ((tag (eieio-object-class client))
         (handler pdd-default-error-handler)
         (url-user-agent (or (oref client user-agent) pdd-user-agent))
         (url-proxy-services (or (oref client proxy-services) url-proxy-services))
         (formdatap (and (consp data)
                         (or (string-match-p
                              "multipart/formdata"
                              (or (alist-get "Content-Type" headers nil nil #'string-equal-ignore-case) ""))
                             (cl-some (lambda (x) (consp (cdr x))) data))))
         (url-request-data (funcall (if (atom data) #'identity ; string
                                      (if formdatap #'pdd-format-formdata #'pdd-format-params)) ; alist
                                    data))
         (url-request-extra-headers (progn
                                      (when formdatap
                                        (setf (alist-get "Content-Type" headers nil nil #'string-equal-ignore-case)
                                              (concat "multipart/form-data; boundary=" pdd-multipart-boundary)))
                                      headers))
         (url-request-method (string-to-unibyte (upcase (format "%s" method))))
         (url-mime-encoding-string "identity")
         (get-resp-content (lambda ()
                             (set-buffer-multibyte (not (pdd-binary-type-p url-http-content-type)))
                             (list (buffer-substring-no-properties
                                    (min (1+ url-http-end-of-headers) (point-max)) (point-max))
                                   (pdd-extract-http-headers)
                                   url-http-response-status
                                   url-http-response-version))))
    (when pdd-debug
      (pdd-log tag "%s %s" url-request-method url)
      (pdd-log tag "HEADER: %S" url-request-extra-headers)
      (pdd-log tag "DATA: %s" url-request-data)
      (pdd-log tag "Proxy: %s" url-proxy-services)
      (pdd-log tag "User Agent: %s" url-user-agent)
      (pdd-log tag "MIME Encoding: %s" url-mime-encoding-string))
    ;; sync
    (if sync
        (condition-case err
            (let ((buf (url-retrieve-synchronously url t)))
              (unwind-protect
                  (with-current-buffer buf
                    (let ((s (funcall get-resp-content)))
                      (if done (pdd-funcall done s) (car s))))
                (ignore-errors (kill-buffer buf))))
          (error (if fail (funcall fail err)
                   (if handler (funcall handler err)
                     (signal 'user-error (cdr err))))))
      ;; async
      (let ((buf (url-retrieve url
                               (lambda (status)
                                 (let ((cb (current-buffer)))
                                   (remove-hook 'after-change-functions #'pdd-url-http-extra-filter t)
                                   (unwind-protect
                                       (if-let* ((err (or (cdr-safe (plist-get status :error))
                                                          (when (or (null url-http-end-of-headers) (= 1 (point-max)))
                                                            (list 'empty-response "Nothing response from server")))))
                                           (if fail (funcall fail err)
                                             (if handler (funcall handler err)
                                               (signal 'user-error err)))
                                         (when done
                                           (pdd-funcall done (funcall get-resp-content))))
                                     (kill-buffer cb))))
                               nil t)))
        (when (and filter (buffer-live-p buf))
          (with-current-buffer buf
            (setq-local pdd-url-extra-filter filter)
            (add-hook 'after-change-functions #'pdd-url-http-extra-filter nil t)))
        (get-buffer-process buf)))))


;;; Implement of plz.el

(defclass pdd-plz-client (pdd-client)
  ((extra-args
    :initarg :args
    :type list
    :documentation "Extra arguments passed to curl program."))
  :documentation "Http Client implemented using `plz.el'.")

(defvar plz-curl-program)
(defvar plz-curl-default-args)
(defvar plz-http-end-of-headers-regexp)
(defvar plz-http-response-status-line-regexp)

(declare-function plz "ext:plz.el" t t)
(declare-function plz-error-p "ext:plz.el" t t)
(declare-function plz-error-message "ext:plz.el" t t)
(declare-function plz-error-curl-error "ext:plz.el" t t)
(declare-function plz-error-response "ext:plz.el" t t)
(declare-function plz-response-status "ext:plz.el" t t)
(declare-function plz-response-body "ext:plz.el" t t)

(defvar pdd-plz-initialize-error-message
  "\n\nTry to install curl and specify the program like this to solve the problem:\n
  (setq plz-curl-program \"c:/msys64/usr/bin/curl.exe\")\n
Or switch http client to `pdd-url-client' instead:\n
  (setq pdd-default-client (pdd-url-client))")

(cl-defmethod pdd :before ((_ pdd-plz-client) &rest _)
  "Check if `plz.el' is available."
  (unless (and (require 'plz nil t) (executable-find plz-curl-program))
    (error "You should have `plz.el' and `curl' installed before using `pdd-plz-client'")))

(cl-defmethod pdd ((client pdd-plz-client) url &key method headers data filter done fail sync retry)
  "Send a request with CLIENT.
See the generic method for args URL, METHOD, HEADERS, DATA, FILTER, DONE, FAIL,
SYNC and RETRY and more."
  (ignore retry)
  (let* ((tag (eieio-object-class client))
         (handler pdd-default-error-handler)
         (plz-curl-default-args (if (slot-boundp client 'extra-args)
                                    (append (oref client extra-args) plz-curl-default-args)
                                  plz-curl-default-args))
         (formdatap (and (consp data)
                         (or (string-match-p
                              "multipart/formdata"
                              (or (alist-get "Content-Type" headers nil nil #'string-equal-ignore-case) ""))
                             (cl-some (lambda (x) (consp (cdr x))) data))))
         (data (funcall (if (atom data) #'identity ; string
                          (if formdatap #'pdd-format-formdata #'pdd-format-params)) ; alist
                        data))
         (string-or-binary (lambda () ; decode according content-type. there is no builtin way to do this in plz
                             (widen)
                             (goto-char (point-min))
                             (unless (looking-at plz-http-response-status-line-regexp)
                               (signal 'plz-http-error
                                       (list "Unable to parse HTTP response status line"
                                             (buffer-substring (point) (line-end-position)))))
                             (let* ((http-version (string-to-number (match-string 1)))
                                    (status-code (string-to-number (match-string 2)))
                                    (headers (pdd-extract-http-headers))
                                    (content-type (alist-get 'content-type headers))
                                    (binaryp (pdd-binary-type-p content-type)))
                               (set-buffer-multibyte (not binaryp))
                               (goto-char (point-min))
                               (unless (re-search-forward plz-http-end-of-headers-regexp nil t)
                                 (signal 'plz-http-error '("Unable to find end of headers")))
                               (narrow-to-region (point) (point-max))
                               (unless binaryp (decode-coding-region (point-min) (point-max) 'utf-8))
                               (list (buffer-string) headers status-code http-version))))
         (raise-error (lambda (err)
                        (when (and (consp err) (memq (car err) '(plz-http-error plz-curl-error)))
                          (setq err (caddr err)))
                        (when (plz-error-p err)
                          (setq err
                                (or (plz-error-message err)
                                    (when-let* ((curl (plz-error-curl-error err)))
                                      (list 'curl-error
                                            (concat (format "%s" (or (cdr curl) (car curl)))
                                                    (pcase (car curl)
                                                      (2 (when (memq system-type '(cygwin windows-nt ms-dos))
                                                           pdd-plz-initialize-error-message))))))
                                    (when-let* ((resp (plz-error-response err)))
                                      (list 'http (plz-response-status resp) (plz-response-body resp))))))
                        (if fail (funcall fail err)
                          (if handler (funcall handler err)
                            (signal 'user-error (cdr err)))))))
    ;; headers
    (when formdatap
      (setf (alist-get "Content-Type" headers nil nil #'string-equal-ignore-case)
            (concat "multipart/form-data; boundary=" pdd-multipart-boundary)))
    (unless (alist-get "User-Agent" headers nil nil #'string-equal-ignore-case)
      (push `("User-Agent" . ,(or (oref client user-agent) pdd-user-agent)) headers))
    ;; log
    (when pdd-debug
      (pdd-log tag "%s" url)
      (pdd-log tag "HEADER: %s" headers)
      (pdd-log tag "DATA: %s" data)
      (pdd-log tag "EXTRA: %s" plz-curl-default-args))
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
              (if done (pdd-funcall done r) (car r)))
          (error (funcall raise-error err)))
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
        :then (lambda (res) (if done (pdd-funcall done res)))
        :else (lambda (err) (funcall raise-error err))))))



(defvar pdd-default-client
  (if (and (require 'plz nil t) (executable-find plz-curl-program))
      (pdd-plz-client)
    (pdd-url-client))
  "Client used by `pdd' by default.
This should be instance of symbol `pdd-client', or a function with current
url or url+method as arguments that return an instance.  If is a function,
the client be used will be determined dynamically when the `pdd' be called.")

(defun pdd-ensure-default-client (args)
  "Pursue the value of variable `pdd-default-client' if it is a function.
ARGS should be the arguments of function `pdd'."
  (if (functionp pdd-default-client)
      (pcase (car (func-arity pdd-default-client))
        (1 (funcall pdd-default-client (car args)))
        (2 (funcall pdd-default-client (car args)
                    (intern-soft
                     (or (plist-get (cdr args) :method)
                         (if (plist-get (cdr args) :data) 'post 'get)))))
        (_ (user-error "If `pdd-default-client' is a function, it can only have
one argument (url) or two arguments (url method)")))
    pdd-default-client))

(defun pdd-fill-keywords-for-args (args)
  "If keywords :done or :data omitted in ARGS, complete them."
  (unless (keywordp (cadr args))
    (if (functionp (cadr args))
        (push :done (cdr args))
      (when (functionp (caddr args))
        (push :done (cddr args)))
      (push :data (cdr args))))
  args)

;;;###autoload
(cl-defmethod pdd (&rest args)
  "Send a request with `pdd-default-client'.
In this case, the first argument in ARGS should be url instead of client.
See the generic method for other ARGS and details."
  (let ((client (pdd-ensure-default-client args)))
    (unless (and client (eieio-object-p client) (object-of-class-p client 'pdd-client))
      (user-error "Make sure `pdd-default-client' is available.  eg:\n
(setq pdd-default-client (pdd-url-client))\n\n\n"))
    (apply #'pdd client (pdd-fill-keywords-for-args args))))

(provide 'pdd)

;;; pdd.el ends here
