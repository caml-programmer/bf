;; configure

(define-syntax ac-configure
  (syntax-rules ()
    ((_)             (ml-ac-configure ()))
    ((_ (e1 e2) ...) (ml-ac-configure '((e1 e2) ...)))
    ((_ (e1 e2))     (ml-ac-configure '((e1 e2))))
    ((_ e1 e2)       (ml-ac-configure '((e1 e2))))
    ((_ (e))         (ml-ac-configure '((e ()))))
    ((_ e)           (ml-ac-configure '((e ()))))))

;; tested:
;; (ac-configure)
;; (ac-configure prefix)
;; (ac-configure prefix "/usr")
;; (ac-configure (prefix))
;; (ac-configure (prefix "/usr"))
;; (ac-configure (prefix "/usr") (libdir "/usr/lib"))

;; make

(define-syntax gnu-make
  (syntax-rules ()
    ((_ (e) (e1 e2) ...) (ml-make '((e ()) (e1 e2) ...)))
    ((_ (e1 e2) ...) (ml-make '((e1 e2) ...)))
    ((_ (e1 e2))     (ml-make '((e1 e2))))
    ((_ e1 e2)       (ml-make '((e1 e2))))
    ((_ (e))         (ml-make '((e ()))))
    ((_ e)           (ml-make '((e ()))))
    ((_)             (ml-make ()))))

;; tested:
;; (make (install) (root top-dir))

;; export

(define-syntax export
  (syntax-rules ()
    ((_)             (ml-export ()))
    ((_ (e1 e2) ...) (ml-export '((e1 e2) ...)))
    ((_ (e1 e2))     (ml-export '((e1 e2))))
    ((_ e1 e2)       (ml-export '((e1 e2))))
    ((_ (e))         (ml-export '((e ()))))
    ((_ e)           (ml-export '((e ()))))))

;; tested:
;; (export)
;; (export HOST)
;; (export HOST "localhost")
;; (export (HOST))
;; (export (HOST "localhost"))
;; (export (HOST "localhost") (PORT "22"))






