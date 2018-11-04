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


*)
      
  