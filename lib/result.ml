type ('ok, 'err) t =
  | Ok of 'ok
  | Error of 'err

let return x = Ok x

let bind m f = match m with
  | Ok x -> f x
  | Error x -> Error x

let (>>=) = bind

let fail x = Error x

