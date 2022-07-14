;; Interface used for wrapping/unwrapping KDX into other 1:1 equivalent tokens.

(namespace (read-msg 'ns))

(interface special-accounts-v2
  (defun assign-special:string (type:module{fungible-v2,supply-control-v1} account:string))
  (defun resolve-special:string (type:module{fungible-v2,supply-control-v1}))
  (defun wrap-transfer:string (type:module{fungible-v2,supply-control-v1} sender:string receiver:string amount:decimal))
  (defun unwrap-transfer:string (type:module{fungible-v2,supply-control-v1} sender:string receiver:string receiver-guard:guard amount:decimal))

  (defcap WRAP:bool
    ( type:module{fungible-v2,supply-control-v1}
      sender:string
      receiver:string
      amount:decimal
    )
    @managed amount WRAP-mgr)

  (defun WRAP-mgr:decimal
    ( managed:decimal
      requested:decimal
    )
  )
  (defcap UNWRAP:bool
    ( type:module{fungible-v2,supply-control-v1}
      sender:string
      receiver:string
      amount:decimal
    )
    @managed amount UNWRAP-mgr)

  (defun UNWRAP-mgr:decimal
    ( managed:decimal
      requested:decimal
    )
  )
)
