[![License: GPL-3.0](http://img.shields.io/:license-gpl3-blue.svg)](https://opensource.org/licenses/GPL-3.0)

raq is an HTTP Library Adapter for Emacs. It support `url.el` and [plz.el](https://github.com/alphapapa/plz.el), and can be extended.

- Its API is simple and uniform
- Support both **sync/async** request
- Support **streaming** request
- Support **retry** for timeout
- Support **proxies** configuration
- Support file **upload/download**

## Installation

Just download the `raq.el` and place it in your `load-path`.

> Notice: package `plz` is optionally. At present, if you prefer to use `curl` to send requests, make sure both `curl` and `plz` are available first.

## Usage

Just request through `raq`, with or without specifying an http client:
``` emacs-lisp
(raq "https://httpbin.org/user-agent" ...)
(raq (raq-plz-client) "https://httpbin.org/user-agent" ...)

;; If request with no http client specified, the request will be sent
;; through client specified by `raq-default-client'.

;; You can config it. If not, it will use `(raq-plz-client)` if possible,
;; then fallback to `(raq-url-client)` if `plz` is unavailable.
(setq raq-default-client (raq-url-client))
(setq raq-default-client (raq-plz-client :args '("--proxy" "socks5://127.0.0.1:1080")))
(setq raq-default-client (raq-url-client :proxies '(("http"  . "host:9999")
                                                    ("https" . "host:9999"))))

;; Use a function to dynamically determine which client to use for a request
;; The function can have one argument (url), or two arguments (url method)
(setq raq-default-client
      (lambda (url)
        (if (string-match-p "deepl.com/" url)
            (raq-plz-client :args '("--proxy" "socks5://127.0.0.1:1080"))
          (raq-plz-client))))
(setq raq-default-client
      (lambda (_ method)
        (if (eq method 'patch) (raq-url-client) (raq-plz-client))))
```

And try to send requests like this:
``` emacs-lisp
;; By default, sync, get
(message "%s" (raq "https://httpbin.org/user-agent"))

;; Use :headers keyword to supply data sent in http header
;; Use :data keyword to supply data sent in http body
;; If :data is present, the :method 'post can be ignored
(raq "https://httpbin.org/post"
     :headers '(("Content-Type" . "application/json"))
     :data '(("key" . "value")) ; or string "key=value&..." directly
     :method 'post)

;; If :done is present and :sync t is absent, the request will be asynchronous!
(raq "https://httpbin.org/post"
     :headers '(("Content-Type" . "application/json"))
     :data '(("key" . "value"))
     :done (lambda (res) (tooltip-show res)))

;; And with :fail to catch the error
(raq "https://httpbin.org/post"
     :headers '(("Content-Type" . "application/json"))
     :data '(("key" . "value"))
     :done (lambda (res) (tooltip-show res))
     :fail (lambda (err) (message "FAIL")))

;; Use :retry to set times auto resend the request if timeout (for async only)
(raq "https://httpbin.org/post"
     :headers '(("Content-Type" . "application/json"))
     :data '(("key" . "value"))
     :done (lambda (res) (tooltip-show res))
     :fail (lambda (err) (message "FAIL"))
     :retry 3)

;; Use :filter to provide logic as every chunk back (for stream feature)
(raq "https://httpbin.org/post"
     :headers '(("Content-Type" . "application/json"))
     :data '(("key" . "value"))
     :filter (lambda () (message "%s" (buffer-size)))
     :done (lambda (res) (tooltip-show res))
     :fail (lambda (err) (message "FAIL")))

;; Arguments of :done are smart, it can be zero, one, two, three or four
(raq "https://httpbin.org/ip" :done (lambda () (message "bingo")))
(raq "https://httpbin.org/ip" :done (lambda (body) (message "%s" body)))
(raq "https://httpbin.org/ip" :done (lambda (_body headers) (message "%s" headers)))
(raq "https://httpbin.org/ip" :done (lambda (_ _ status-code) (message "%s" status-code)))
(raq "https://httpbin.org/ip" :done (lambda (_ _ _ http-version) (message "%s" http-version)))

;; Specific method
(raq "https://httpbin.org/uuid")
(raq "https://httpbin.org/patch" :method 'patch)
(raq "https://httpbin.org/delete" :method 'delete)

;; Upload. Notice the difference: for file, not (a . path), but a list
;; like (name path) or (name path mime-type)
(raq "https://httpbin.org/post"
     :data '((key1 . "hello")
             (key2 . "world")
             (file1 "~/aaa.xxx")
             (file2 "~/aaa.png" "image/png")))

;; Download
(with-temp-file "~/aaa.jpeg"
  (insert (raq "https://httpbin.org/image/jpeg")))
```

## API

``` emacs-lisp
(cl-defgeneric raq (raq-client url &rest _args &key method headers data
                                filter done fail sync retry &allow-other-keys)
  "Send HTTP request using the given RAQ-CLIENT.

Keyword arguments:
  - URL: The URL to send the request to.
  - METHOD: Request method, symbol like 'post. If nil guess by data.
  - HEADERS: Additional headers to include in the request. Alist.
  - DATA: The data to include in the request. If this is a string, it will be
          sent directly as request body. If this is a list and every element
          is (key . value) then this will be joined to a string like a=1&b=2 and
          then be sent. If this is a list and some element is (key filename)
          format, then the list will be normalized as multipart formdata string
          and be sent.
  - FILTER: A function to be called every time when some data returned.
  - DONE: A function to be called when the request succeeds.
  - FAIL: A function to be called when the request fails.
  - RETRY: How many times it can retry for timeout. Number.
  - SYNC: Non-nil means request synchronized. Boolean.

If request async, return the process behind the request."）
```

## Miscellaneous

Issues and PRs are welcome. Happy good day.
