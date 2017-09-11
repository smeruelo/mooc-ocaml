type date =
  { year : int; month : int; day : int; hour : int; minute : int }

let the_origin_of_time =
  { year = 1; month = 1; day = 1; hour = 0; minute = 0 }

let wellformed {year; month; day; hour; minute} =
  year >= 1 && month >= 1 && month <= 5 && day >=1 && day <= 4 &&
    hour >= 0 && hour <= 2 && minute >= 0 && minute <= 1

let next {year; month; day; hour; minute} =
  if minute = 0 then
    {year; month; day; hour; minute = 1}
  else if hour < 2 then
    {year; month; day; hour = hour + 1; minute = 0}
  else if day < 4 then
    {year; month; day = day + 1; hour = 0; minute = 0}
  else if month < 5 then
    {year; month = month + 1; day = 1; hour = 0; minute = 0}
  else
    {year = year + 1; month = 1; day = 1; hour = 0; minute = 0}

let of_int m =
  let minutes_in_hour = 2 in
  let minutes_in_day = 3 * minutes_in_hour in
  let minutes_in_month = 4 * minutes_in_day in
  let minutes_in_year = 5 * minutes_in_month in
  let year = m / minutes_in_year + 1
  and m = m mod minutes_in_year in
  let month = m / minutes_in_month + 1
  and m = m mod minutes_in_month in
  let day = m / minutes_in_day + 1
  and m = m mod minutes_in_day in
  let hour = m / minutes_in_hour
  and minute = m mod minutes_in_hour
  in {year; month; day; hour; minute}
