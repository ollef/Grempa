main x y (X z) (Y a) = case z of
    { X z -> f x
    ; Y z -> f z
    };

f x = let g (X x) = x in g 100
