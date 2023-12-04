let rec calculate_calibration_sum lines total_sum =
  match lines with
  | [] -> total_sum
  | line :: rest ->
      (* Function to extract the first and last digits *)
      let extract_digits s =
        (int_of_char s.[0] - int_of_char '0', int_of_char s.[String.length s - 1] - int_of_char '0')
      in
      (* Extract the first and last digits from the current line *)
      let first_digit, last_digit = extract_digits line in
      (* Combine the first and last digits into a two-digit number *)
      let calibration_value = first_digit * 10 + last_digit in
      (* Recursively calculate the sum for the rest of the lines *)
      calculate_calibration_sum rest (total_sum + calibration_value)

(* Example calibration document *)
let calibration_document =
  "1abc2\n\
   pqr3stu8vwx\n\
   a1b2c3d4e5f\n\
   treb7uchet"

let () =
  (* Split the calibration document into lines *)
  let lines = String.split_on_char '\n' calibration_document in
  (* Calculate the sum of calibration values *)
  let result = calculate_calibration_sum lines 0 in
  (* Print the result *)
  Printf.printf "%d\n" result
