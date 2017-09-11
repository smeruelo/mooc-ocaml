let last_character str =
  String.get str (String.length str - 1)

let string_of_bool truth =
  if truth then "true" else "false"
