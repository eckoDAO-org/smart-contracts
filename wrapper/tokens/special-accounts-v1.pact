;; Interface used for wrapping/unwrapping KDX into other 1:1 equivalent tokens.

(namespace (read-msg 'ns))

(interface special-accounts-v1
  (defun assign-special:string (name:string account:string))
  (defun resolve-special:string (name:string))
  (defun wrap-transfer:string (type:string sender:string receiver:string amount:decimal))
  (defun unwrap-transfer:string (type:string sender:string receiver:string receiver-guard:guard amount:decimal))

  (defcap WRAP:bool
    ( type:string
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
    ( type:string
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
