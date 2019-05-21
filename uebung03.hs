type Nibble = (Bool, Bool, Bool, Bool)
type Byte = (Nibble, Nibble)


shiftByte :: Byte -> Byte
shiftByte ((a,b,c,d),(e,f,g,h))
 = ((b,c,d,False),(f,g,h,False))

komplement :: Nibble -> Nibble
komplement (a,b,c,d)
  | a == True = (a,b,c,d)
  | a == False = (True,b,c,d)

signExtension :: Nibble -> Byte
signExtension (a,b,c,d)
  = ((a,b,c,d),komplement (a,b,c,d))

multiBitByte :: Bool -> Byte -> Byte
multiBitByte f (a,b)
  | f == False = (signExtension(False,False,False,False))
  | f == True =  (a,b)

xor :: Bool -> Bool -> Bool
xor True False = True
xor False True = True
xor _ _ = False

fulladder :: Bool -> Bool -> Bool ->(Bool, Bool)
fulladder a b c =  ((c && (xor a b)) || (a&&b) , xor (xor a b) c)

rippleCarryAdder :: Nibble -> Nibble -> Nibble
rippleCarryAdder (a,b,c,d) (w,x,y,z) = (snd(fulladder a w (fst(fulladder b x (fst(fulladder c y (fst(fulladder d z False))))))), snd(fulladder b x (fst(fulladder c y (fst(fulladder c z False ))))), snd(fulladder c y (fst(fulladder d z False))), snd(fulladder d z False))


rippleCarryAdderByte :: Byte -> Byte -> Byte
rippleCarryAdderByte (a,b) (c,d) = (rippleCarryAdder a c , rippleCarryAdder b d)


--test
aufgabe01 = do
  putStrLn("###### shiftByte ######")
  putStrLn(show(shiftByte((True,True,True,True),(True,True,True,True))))
  putStrLn("###### signExtension ######")
  putStrLn(show(signExtension(False,True,True,True)))
  putStrLn("###### multiBitByte ######")
  putStrLn(show(multiBitByte True (signExtension(False,True,False,True))))
  putStrLn(show(multiBitByte False (signExtension(False,True,False,True))))
  putStrLn("###### rippleCarryAdderByte ######")
  putStrLn(show(rippleCarryAdderByte(signExtension(False,False,True,True))(signExtension(True,False,False,True))))
