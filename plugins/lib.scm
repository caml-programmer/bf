;; configure

(define-syntax ac-configure
  (syntax-rules ()
    ((_ (e1 e2) ...) (ml-ac-configure `((e1 ,e2) ...)))
    ((_ (e1 e2))     (ml-ac-configure `((e1 ,e2))))
    ((_ e1 e2)       (ml-ac-configure `((e1 ,e2))))
    ((_ (e))         (ml-ac-configure `((e ()))))
    ((_ e)           (ml-ac-configure `((e ()))))
    ((_)             (ml-ac-configure `()))))

;; tested:
;; (ac-configure)
;; (ac-configure prefix)
;; (ac-configure prefix "/usr")
;; (ac-configure (prefix))
;; (ac-configure (prefix "/usr"))
;; (ac-configure (prefix "/usr") (libdir "/usr/lib"))
;; (ac-configure (prefix top-dir) (libdir "/usr/lib") (includdir "/usr/include") (good ()) (enable-module "ssl"))

;; make

(define-syntax make
  (syntax-rules ()
    ((_ (e) (e1 e2) ...) (ml-make `((e ()) (e1 ,e2) ...)))
    ((_ (e1 e2) ...)     (ml-make `((e1 ,e2) ...)))
    ((_ (e1 e2))         (ml-make `((e1 ,e2))))
    ((_ e1 e2)           (ml-make `((e1 ,e2))))
    ((_ (e))             (ml-make `((e ()))))
    ((_ e)               (ml-make `((e ()))))
    ((_)                 (ml-make `()))))

;; tested:
;; (make (install) (root top-dir))

;; export

(define-syntax export
  (syntax-rules ()
    ((_ (e1 e2) ...) (ml-export `((e1 ,e2) ...)))
    ((_ (e1 e2))     (ml-export `((e1 ,e2))))
    ((_ e1 e2)       (ml-export `((e1 ,e2))))
    ((_ (e))         (ml-export `((e ()))))
    ((_ e)           (ml-export `((e ()))))
    ((_)             (ml-export `()))))

;; tested:
;; (export)
;; (export HOST)
;; (export HOST "localhost")
;; (export (HOST))
;; (export (HOST "localhost"))
;; (export (HOST "localhost") (PORT "22"))

(define-syntax update-make-params
  (syntax-rules ()
    ((_ (e) (e1 e2) ...) (ml-update-make-params `((e ()) (e1 ,e2) ...)))
    ((_  e  (e1 e2) ...) (ml-update-make-params `((e ()) (e1 ,e2) ...)))))

;; tested:
;; (update-make-params "make.params"
;;                     (SRC_DIR (current-directory))
;;                     (SKVT_HOME (path-concat top-dir "skvt"))
;;                     (SKVT_BASE top-dir)
;;                     (TARGET_FORMAT "zo"))




