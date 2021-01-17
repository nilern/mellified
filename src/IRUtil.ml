type flag = Flex | Rigid

let flag_to_string = function
    | Flex -> ">="
    | Rigid -> "="

let flag_max flag flag' = match (flag, flag') with
    | (Flex, Rigid) -> flag'
    | _ -> flag

