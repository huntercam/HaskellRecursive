import Data.Ord          ( comparing )
import Data.List         ( maximumBy, subsequences )
import Data.List.Ordered ( isSorted, nub )
 
lis :: Ord a => [a] -> [a]
lis = maximumBy (comparing length) . map nub  . filter isSorted . subsequences                 
--    longest                    <-- unique <-- increasing    <-- all      
 
main = do
  print $ lis [3,2,6,4,5,1]
  print $ lis [0,8,4,12,2,10,6,14,1,9,5,13,3,11,7,15]
  print $ lis [1,1,1,1]


tuple(int,string,int)

int DP[tamaño][index][rta];
sml(int tamaño, int index){
	if(DP[tamaño][index] != -1)
		return DP[tamaño][index];

		int a = sml(tamaño-1, index+1);
		int b = sml(tamaño,- index+1);
	return DP [tamaño][index] = max(a,b);	
}

sml( string a, int index, int rta[]){
	
if(DP[index][rta] != -1)
return DP[index][Hash[rta]];

	if(rta[ rta.size() -1] <= a[index] || rta = "" ){
		int rama1 = 1 + sml(a,index+1, rta + a[index]);
		int rama2 =  sml(a,index+1,rta);  
		return DP[index][Hash[rta]] = max(rama1,rama2);
	}else{
		return DP[index][Hash[rta]] = sml(a,index+1,rta);	
	}

dp!!i$!!j$!!r

(|||) f g = (f,g)
let (rama1,rama2) = f ||| g

a = 4 2 1 10 3 5 6

rama1 = 1 + 
rta = 4

rama2 =   
rta = 2

}



[1,2,3,4]



LIS (string s, int index, int max){
	if(DP[index][max] == -1)
		return DP[index][max];

	if(s[index] >= max){
		int a = 1 + LIS(s,index+1,s[index]);
		int b = LIS(s,index+1,max);
		return DP[index][max] = max(a,b);
	}else{
		return DP[index][max] = LIS(s,index+1,max);	
	}
}



