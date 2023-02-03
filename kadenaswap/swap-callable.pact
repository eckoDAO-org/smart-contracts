;; This interface represents an API used for extending swap calls with user code
;; that can run after a swap has been performance in the exchange. Used by the
;; low-level `swap` function in `exchange.pact`.

(namespace (read-msg 'ns))

(interface swap-callable-v1
  " API for receiving a callback after a swap leg transfer \
  \ but before constant-product invariants are enforced."
  (defun swap-call:bool
    ( token-in:module{fungible-v2}
      token-out:module{fungible-v2}
      amount-out:decimal
      sender:string
      recipient:string
      recipient-guard:guard
    )
    " Operate on an optimistic swap of AMOUNT-OUT of TOKEN-OUT \
    \ from SENDER to RECIPIENT/RECIPIENT-GUARD. TOKEN-IN is provided \
    \ to give the pair context as well as the inbound amount swapped. \
    \ Boolean result value is ignored."
  )
)

(module noop-callable GOVERNANCE
  "Noop implementation of swap-callable-v1"
  (implements swap-callable-v1)
  (defcap GOVERNANCE () (enforce-guard (keyset-ref-guard 'kaddex-exchange-admin)))
  (defun swap-call:bool
    ( token-in:module{fungible-v2}
      token-out:module{fungible-v2}
      amount-out:decimal
      sender:string
      recipient:string
      recipient-guard:guard
    )
    "Noop implementation"
    true
  )
)
