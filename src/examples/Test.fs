main x y (X z) (Y a) = case z of
    { X z -> f x
    ; Y z -> f z
    };

f x = let g (X x) = x in 123 + g 100 (main x) * 12
