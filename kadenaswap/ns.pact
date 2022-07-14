(define-keyset 'kaddex-ns-user)
(define-keyset 'kaddex-ns-admin)
(ns.write-registry (read-msg 'ns) (keyset-ref-guard 'kaddex-ns-admin) true)
(define-namespace
  (read-msg 'ns)
  (keyset-ref-guard 'kaddex-ns-user)
  (keyset-ref-guard 'kaddex-ns-admin)
)
