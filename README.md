[![License: GPL-3.0](http://img.shields.io/:license-gpl3-blue.svg)](https://opensource.org/licenses/GPL-3.0)

pdd is an HTTP Library Adapter for Emacs. It support `url.el` and [plz.el](https://github.com/alphapapa/plz.el), and can be extended.

- Its API is simple and uniform
- Support both **sync/async** request
- Support **streaming** request
- Support **retry** for timeout
- Support **proxies** configuration
- Support file **upload/download**

> In my language, pdd is the meaning of "get the thing you want quickly"

## Installation

Just download the `pdd.el` and place it in your `load-path`.

> Notice: package `plz` is optionally. At present, if you prefer to use `curl` to send requests, make sure both `curl` and `plz` are available first.

## Usage

Just request through `pdd`, with or without specifying an http client:
``` emacs-lisp
(pdd "https://httpbin.org/user-agent" ...)
(pdd (pdd-plz-client) "https://httpbin.org/user-agent" ...)

;; If request with no http client specified, the request will be sent
;; through client specified by `pdd-default-client'.

;; You can config it. If not, it will use `(pdd-plz-client)` if possible,
;; then fallback to `(pdd-url-client)` if `plz` is unavailable.
(setq pdd-default-client (pdd-url-client))
(setq pdd-default-client (pdd-plz-client :args '("--proxy" "socks5://127.0.0.1:1080")))
(setq pdd-default-client (pdd-url-client :proxies '(("http"  . "host:9999")
                                                    ("https" . "host:9999"))))

;; Use a function to dynamically determine which client to use for a request
;; The function can have one argument (url), or two arguments (url method)
(setq pdd-default-client
      (lambda (url)
        (if (string-match-p "deepl.com/" url)
            (pdd-plz-client :args '("--proxy" "socks5://127.0.0.1:1080"))
          (pdd-plz-client))))
(setq pdd-default-client
      (lambda (_ method)
        (if (eq method 'patch) (pdd-url-client) (pdd-plz-client))))
```

And try to send requests like this:
``` emacs-lisp
;; By default, sync, get
(pp (pdd "https://httpbin.org/user-agent"))

;; Use :headers keyword to supply data sent in http header
;; Use :data keyword to supply data sent in http body
;; If :data is present, the :method 'post can be ignored
(pdd "https://httpbin.org/post"
  :headers '(("User-Agent" . "..."))
  :data '(("key" . "value")) ; or string "key=value&..." directly
  :method 'post)

;; If :done is present and :sync t is absent, the request will be asynchronous!
(pdd "https://httpbin.org/post"
  :data '(("key" . "value"))
  :done (lambda (res) (pp res)))

;; And with :fail to catch the error
(pdd "https://httpbin.org/post"
  :data '(("key" . "value"))
  :done (lambda (res) (pp res))
  :fail (lambda (err) (message "FAIL")))

;; Use `pdd-default-error-handler' to catch error when :fail is absent
;; Set its value globally, or just dynamically bind it with let
(let ((pdd-default-error-handler
       (lambda (err) (message "Crying %s..." (cadr err)))))
  (pdd "https://httpbin.org/post-error"
    :data '(("key" . "value"))
    :done (lambda (res) (pp res))))

;; Use :retry to set times auto resend the request if timeout (for async only)
;; Also, you can see, if the content-type is json, :data will be auto decoded,
;; If the response content-type is json, result string is auto parsed to elisp object
;; So, for RESTful, what you need is just specify the correct json content-type
(pdd "https://httpbin.org/post"
  :params '(("version" . "111"))
  :headers '(("Content-Type" . "application/json"))
  :data '(("key" . "value"))
  :done (lambda (res) (pp res))
  :fail (lambda (err) (message "FAIL"))
  :retry 3)

;; Use :filter to provide logic as every chunk back (for stream feature)
(pdd "https://httpbin.org/post"
  :data '(("key" . "value"))
  :filter (lambda () (message "%s" (buffer-size)))
  :done (lambda (res) (pp res))
  :fail (lambda (err) (message "FAIL")))

;; Arguments of :done are smart, it can be zero, one, two, three or four
;; If zero argument, current buffer is the one with raw responsed string
(pdd "https://httpbin.org/ip" :done (lambda () (message "%s" (buffer-string))))
(pdd "https://httpbin.org/ip" :done (lambda (body) (message "IP: %s" (cdar body))))
(pdd "https://httpbin.org/ip" :done (lambda (_body headers) (message "%s" headers)))
(pdd "https://httpbin.org/ip" :done (lambda (_ _ status-code) (message "%s" status-code)))
(pdd "https://httpbin.org/ip" :done (lambda (_ _ _ http-version) (message "%s" http-version)))

;; Specific method
(pdd "https://httpbin.org/uuid")
(pdd "https://httpbin.org/patch" :method 'patch)
(pdd "https://httpbin.org/delete" :method 'delete)

;; Upload. Notice the difference: for file, not (a . path), but a list
;; like (name path) or (name path mime-type)
(pdd "https://httpbin.org/post"
  :data '((key1 . "hello")
          (key2 . "world")
          (file1 "~/aaa.xxx")
          (file2 "~/aaa.png" "image/png")))

;; Download
(with-temp-file "~/aaa.jpeg"
  (insert (pdd "https://httpbin.org/image/jpeg")))
```

## API

``` emacs-lisp
(cl-defgeneric pdd (pdd-client url &rest _args &key method params headers data resp
                                                    filter done fail sync retry
                                                    &allow-other-keys)
  "Send HTTP request using the given PDD-CLIENT.

Keyword arguments:
  - URL: The URL to send the request to.
  - PARAMS: The data to include in the url.  It's a string or alist.
  - METHOD: Request method, symbol like 'post.  If nil guess by data.
  - HEADERS: Additional headers to include in the request.  Alist.
  - DATA: The data to include in the request.  If this is a string, it will be
          sent directly as request body.  If this is a list and every element
          is (key . value) then this will be joined to a string like a=1&b=2 and
          then be sent.  If this is a list and some element is (key filename)
          format, then the list will be normalized as multipart formdata string
          and be sent.
  - RESP: Whether or how to auto encode the response content.
          Currently this should a function with responsed string as argument.
          For example, make this with value #'identity should make
          the raw responsed string is passed to DONE without any parsed.
  - FILTER: A function to be called every time when some data returned.
  - DONE: A function to be called when the request succeeds.
  - FAIL: A function to be called when the request fails.
  - RETRY: How many times it can retry for timeout.  Number.
  - SYNC: Non-nil means request synchronized.  Boolean.

If request async, return the process behind the request."）
```

## Miscellaneous

Issues and PRs are welcome. Happy good day.
