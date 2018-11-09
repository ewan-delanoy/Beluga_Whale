(*

#use"Text_editing/improve_readiris_ocr.ml";;

*)

let replace_fixed_length_pattern_with_constant_in_string
   pattern_tester 
     pattern_length 
      replacement 
        argument_string=
    let n=String.length argument_string in 
    let cases = List.filter (pattern_tester argument_string) (Ennig.ennig 2 (n-pattern_length)) in 
    if cases=[] then argument_string else
    let temp1=(1-pattern_length)::(cases@[n+1]) in 
    let temp2=Listennou.universal_delta_list temp1 in 
    let temp3=Image.image (fun (a,b)->
        let unchanged_part=(
            let i= a+pattern_length
            and j= b-1 in 
            if i>j then "" else
            Cull_string.interval argument_string (a+pattern_length) (b-1) 
        ) in 
        if b=n+1 then unchanged_part else unchanged_part^replacement
    ) temp2 in
    String.concat "" temp3;; 

let replace_fixed_length_pattern_with_constant_in_file
   pattern_tester 
     pattern_length 
      replacement 
        argument_file=
    let old_text=Io.read_whole_file argument_file in 
    let new_text=replace_fixed_length_pattern_with_constant_in_string
    pattern_tester pattern_length replacement old_text in 
    Io.overwrite_with argument_file new_text;; 

let modify_words_in_string f s=
  let temp1=Str.full_split (Str.regexp"[ \n\r\t]+")  s in 
  let temp2=Image.image (function
     Str.Delim(delim)->delim
     |Str.Text(text)->f text
  ) temp1 in 
  String.concat "" temp2;;

let  modify_words_in_file f argument_file=
    let old_text=Io.read_whole_file argument_file in 
    let new_text=modify_words_in_string f old_text in 
    Io.overwrite_with argument_file new_text;; 

(*

replace_fixed_length_pattern_with_constant_in_string
   (fun s k->
       if k<2 then false else
       (Strung.get s k=';')&&
       (List.mem (Strung.get s (k-1)) Charset.alphanumeric_characters)
   )
     1
      " ;" 
        "abc;def ;gh ;ijk ;lmn";;

let is_illegal s=
  let n=String.length(s) in 
  let tempf1=(fun l k->
    List.mem (Strung.get s k) l
  ) in 
  let tempf2=(fun l i j->List.exists(tempf1 l)(Ennig.ennig i j)) in 
  if tempf1 Charset.lowercase_letters 1
  then tempf2 Charset.uppercase_letters 2 n 
  else (tempf2 Charset.lowercase_letters 2 n)&&
       (tempf2 Charset.uppercase_letters 2 n);;

let make_legal s= 
  if is_illegal s then String.lowercase_ascii s else s;;

modify_words_in_string make_legal "aBc\t\nDe\n\t\tFGh klmp";;
  


*)
      
let isolate_character_in_string c s=
    let sc=(String.make 1 c) and n=String.length(s) in 
    let temp1=replace_fixed_length_pattern_with_constant_in_string
   (fun s k->
       if k<2 then false else
       (Strung.get s k=c)&&
       (not(List.mem (Strung.get s (k-1)) Charset.list_of_whites))
   )
     1  (" "^sc) s in 
     replace_fixed_length_pattern_with_constant_in_string
   (fun s k->
       if k>=n then false else
       (Strung.get s k=c)&&
       (not(List.mem (Strung.get s (k+1)) Charset.list_of_whites))
   )
     1 (sc^" ") temp1;;

(*

isolate_in_string ';' "abc;def; gh ;ijk ; lmolp"

*)

let  isolate_character_in_file c argument_file=
    let old_text=Io.read_whole_file argument_file in 
    let new_text=isolate_character_in_string c  old_text in 
    Io.overwrite_with argument_file new_text;; 


