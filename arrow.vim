syntax match Comment "#.*$"
syntax match Function "=>"
syntax match TypeDef "\~>"
syntax match TypeDef "\v'(\w|-)+"
syntax region String start=/\v"/ skip=/\v\\./ end=/\v"/
