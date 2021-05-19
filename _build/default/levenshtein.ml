let helper dist1 dist2 s1 s2 edit_table =
  for i = 0 to dist1 do
    for j = 0 to dist2 do
      if i = 0 then Hashtbl.add edit_table (i, j) j
      else if j = 0 then Hashtbl.add edit_table (i, j) i
      else if s1.[i - 1] = s2.[j - 1] then
        Hashtbl.add edit_table (i, j)
          (Hashtbl.find edit_table (i - 1, j - 1))
      else
        let comp1 = Hashtbl.find edit_table (i, j - 1) in
        let comp2 = Hashtbl.find edit_table (i - 1, j) in
        let comp3 = Hashtbl.find edit_table (i - 1, j - 1) in
        let min1 = if comp1 > comp2 then comp2 else comp1 in
        let min2 = if min1 > comp3 then comp3 else min1 in
        Hashtbl.add edit_table (i, j) (1 + min2)
    done
  done

let dist string1 string2 =
  let length1 = String.length string1 in
  let length2 = String.length string2 in
  let our_table = Hashtbl.create (length1 * length2) in
  helper length1 length2 string1 string2 our_table;
  Hashtbl.find our_table (length1, length2)
