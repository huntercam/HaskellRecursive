import Data.Array
uno(x,y,z)=x
dos(x,y,z)=y
tres(x,y,z)=z

generar1dim 0 = []
generar1dim n = [0]++(generar1dim (n-1))

generar2dim n 0  = []
generar2dim n m = [(generar1dim n)] ++  generar2dim n (m-1)

cambiarf 0 e (x:xs) = (e:xs)
cambiarf j e (x:xs) = x : cambiarf (j-1) e xs

cambiarc 0 j e (xs:xss) = cambiarf j e xs : xss
cambiarc i j e (xs:xss) = xs : cambiarc (i-1) j e xss




--n = length(s);
--m = maxNum(s);


generarArray n m = generar2dim n m 



dp index [] maximo pd = (pd,0)
dp index (x:xs) maximo pd =
	if pd!!index!!maximo == 0 then
		if x >= maximo then
			let 
		                optimo = max a b
				kk = (cambiarc index maximo optimo pd)	
				aa = ( dp (index+1) xs x kk)
				a = 1 +  (snd aa)
				bb = (dp (index+1) xs maximo kk)
				b = (snd bb)
			in
			if optimo == a then
			( fst (aa) ,optimo)
			else
			( fst (bb) ,optimo) -- pd[index][max] = max(a,b)
		
		else

		let
		kk = (cambiarc index maximo b pd)
		bb = (dp (index+1) xs maximo kk)	
		b = snd (bb)
		in
		( fst(bb) , b) 


	else
        ( pd , pd!!index!!maximo)


maxnum [x] = x
maxnum (x:xs) = if x > maxnum xs then x else maxnum xs 

progdin (x:xs) = dp 0 (x:xs) 0 (generarArray (maxnum (x:xs)+1) (length (x:xs))  )

generarvalor y (x:xs) i = if x == 0 then 1+generarvalor (xs) else 1 - y   

reconstruccion (xs:xss) i = (generarvalor xs i) : reconstruccion xss (i-1)







