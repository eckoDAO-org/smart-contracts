;;Functions for creating namespace util and its admin and users.
(env-exec-config ["DisablePact44"]) ;; necessary for top-level keyset definitions
(define-keyset 'util-ns-users)
(define-keyset 'util-ns-admin)
(env-exec-config [])

(define-namespace 'util
  (keyset-ref-guard 'util-ns-users)
  (keyset-ref-guard 'util-ns-admin))
