module FourColors where
---------------------------------
--- PRIMERA FASE: ADJACENCIES ---
---------------------------------

--- Tipos de datos
type Zone      = Int
type Row       = [Zone]
type Map       = [Row]
type Adjacency = (Zone,[Zone])

--- Función adjacency
--- Obtiene todas las adyacencias del mapa
adjacencies :: Map -> [Adjacency]
adjacencies (r1:m) = adjAux (r1:m) (adjLeftAux r1 []) -- rmEmp (rmRed (adjAux (r1:m) (adjLeftAux r1 [])))
  where
    -- Bucle principal
    adjAux :: Map -> [Adjacency] -> [Adjacency]
    adjAux (r1:r2:m) acc = adjAux (r2:m) (adjLeftAux r2 (adjUpAux r1 r2 acc))
    adjAux [_] acc = acc
    adjAux _ acc = acc
    -- Busca adyacencias arriba de las celdas
    adjUpAux :: Row -> Row -> [Adjacency] -> [Adjacency]
    adjUpAux (x:xs) (y:ys) acc
      | x /= y    = adjUpAux xs ys (addAdj x y acc)
      | otherwise = adjUpAux xs ys acc
    adjUpAux _ _ acc = acc
    -- Busca adyacencias a la izquierda de las celdas
    adjLeftAux :: Row -> [Adjacency] -> [Adjacency]
    adjLeftAux (r1:r2:rs) acc
      | r1 /= r2  = adjLeftAux (r2:rs) (addAdj r1 r2 acc)
      | otherwise = adjLeftAux (r2:rs) acc
    adjLeftAux _ acc = acc
    addAdj :: Zone -> Zone -> [Adjacency] -> [Adjacency]
    addAdj x y adjs
      | x > y = addAdj1 x y adjs
      | y < x = addAdj1 y x adjs
      | otherwise = addAdj1 y x adjs -- No debería ocurrir
      where
        addAdj1 :: Zone -> Zone -> [Adjacency] -> [Adjacency]
        addAdj1 y x ((z,zs):adjs)
          | y == z    = (z,addAdj2 x zs) : adjs
          | y < z     = (y,[x]) : (z,zs) : adjs
          | otherwise = (z,zs) : addAdj1 y x adjs
          where
            addAdj2 :: Zone -> [Zone] -> [Zone]
            addAdj2 x (z:zs)
              | x == z    = z : zs
              | x < z     = x : z : zs
              | otherwise = z : addAdj2 x zs
            addAdj2 x _ = [x]
        addAdj1 y x _ = [(y,[x])]
    -- Elimina redundancias en la lista de adyacencias
    -- rmRed :: [Adjacency] -> [Adjacency]
    -- rmRed (a:as) = rmRedAux a as : rmRed as
    --   where
    --     rmRedAux :: Adjacency -> [Adjacency] -> Adjacency
    --     rmRedAux (a,as) ((b,bs):adjs)
    --       | elem a bs && elem b as = rmRedAux (a, delete b as) adjs
    --       | otherwise              = rmRedAux (a,as) adjs
    --       where
    --         delete :: Zone -> [Zone] -> [Zone]
    --         delete x (a:as)
    --           | x == a    = delete x as -- Mantener recursividad por consistencia
    --           | otherwise = a : delete x as
    --         delete x _ = []
    --     rmRedAux a _ = a
    -- rmRed _ = []
    -- Eliminar las Adjacency sin lista
    rmEmp :: [Adjacency] -> [Adjacency]
    rmEmp ((_,[]):adjs) = rmEmp adjs
    rmEmp ((a,as):adjs) = (a,as) : rmEmp adjs
    rmEmp _ = []

---------------------------
--- SEGUNDA FASE: COLOR ---
---------------------------

--- Tipos de datos
data Color    = Red | Green | Blue | Yellow
  deriving (Enum,Eq,Show)
type Solution = [Color]
type Node     = ([Adjacency],Zone,Zone,Solution)

--- Función color
--- Busca una solución para colorear un mapa con un máximo de cuatro colores
---   -Si existe solución, devuelve un String con la solución
---   -Si no existe solución, devuelve un String indicando que no existe
color :: Map -> String
color map = takeFirst (solution map)
  where --- Función takeFirst
        --- Devuelve la solución contenida en el primer elemento de una lista de nodos solución
        --- Si la lista es vacía, devuelve una cadena indicando que no hay soluciones
        takeFirst :: [Node] -> String
        takeFirst []             = "No solution!\n"
        takeFirst ((_,_,_,c):ss) = "Solution: " ++ show c ++ "\n"

--- Función solution
--- Devuelve TODOS los nodos solución del problema
solution :: Map -> [Node]
solution map = bt esSol comp (initialNode map)

--- Función del esquema de Backtracking
--- Esta función aplica el esquema de Backtracking a cualquier problema
bt :: (a -> Bool) -> (a -> [a]) -> a -> [a]
bt esSol comp node
  | esSol node = [node]
  | otherwise  = concatMap (bt esSol comp) (comp node)

--- Función colorList
--- Devuelve la enumeración de todos los elementos del tipo Color
colorList :: [Color]
colorList = [toEnum 0::Color ..]

-- --- Función esSol
-- --- Comprueba si un nodo es solución o no
esSol :: Node -> Bool
esSol (adjs, num, _, sol) = andAux [1..num] sol adjs
  where
    andAux :: [Zone] -> Solution -> [Adjacency] -> Bool
    andAux (z:zs) sol adjs  = null (sameColor z (adjOf adjs z) sol) && andAux zs sol adjs
    andAux _ _ _            = True

--- Función comp
--- Calcula las compleciones del nodo actual coloreando la zona actual con los colores utilizables
comp :: Node -> [Node]
comp node@(adjs, num, zone, sol) = genNodes node (sameColor zone (adjOf adjs zone) sol)
  where
    genNodes :: Node -> [Zone] -> [Node]
    genNodes node@(adjs, n, z, sol) (zone:zs) -- = genNode (adjs, n, z, sol) zone: genNodes node zs
      | null (posColors adjs sol zone Red) = genNodes node zs
      | otherwise                          = genNode (adjs, n, z, sol) zone: genNodes node zs
      where
        genNode ::  Node -> Zone -> Node
        genNode node@(adjs, n, z, sol) zone = (adjs, n, z, chngColor zone sol (takeFirst (posColors adjs sol zone Red)))
          where
            takeFirst :: [Color] -> Color
            takeFirst (c:cs) = c
            chngColor :: Zone -> Solution -> Color -> Solution
            chngColor z (s:ss) c
              | z == 1    = c : ss
              | otherwise = s : chngColor (z-1) ss c
            chngColor _ sol _ = sol
        posColors :: [Adjacency] -> Solution -> Zone -> Color -> [Color]
        posColors adjs sol z c
          | nextColor c /= Red && null (sameColorAux c (adjOf adjs z) sol)      = c : posColors adjs sol z (nextColor c)
          | nextColor c == Red && null (sameColorAux c (adjOf adjs z) sol)      = [c]
          | nextColor c /= Red && not(null (sameColorAux c (adjOf adjs z) sol)) = posColors adjs sol z (nextColor c)
          | otherwise                                                           = []
    genNodes (adjs, num, zone, sol) []
      | zone < num    = [(adjs,num,zone+1,sol)]
      | otherwise     = []

-- Siguiente color
nextColor :: Color -> Color
nextColor c
  | c == Red    = Green
  | c == Green  = Blue
  | c == Blue   = Yellow
  | otherwise   = Red

-- Zonas adyacentes a zona
adjOf :: [Adjacency] -> Zone -> [Zone]
adjOf ((a,as):adjs) z
  | a == z              = as ++ adjOf adjs z
  | a /= z && elem z as = a  : adjOf adjs z
  | otherwise           = adjOf adjs z
adjOf _ _ = []

sameColor :: Zone -> [Zone] -> Solution -> [Zone]
sameColor z zs sol = sameColorAux (colorOf z sol) zs sol

sameColorAux :: Color -> [Zone] -> Solution -> [Zone]
sameColorAux c (z:zs) sol
  | colorOf z sol == c  = z : sameColorAux c zs sol
  | otherwise             = sameColorAux c zs sol
sameColorAux _ _ _ = []

-- Color de una zona
colorOf :: Zone -> Solution -> Color
colorOf z (s:ss)
  | z == 1    = s
  | otherwise = colorOf (z-1) ss
colorOf z _   = Red -- Por consistencia, no se debería llegar aquí NUNCA

--- Función initialNode
--- Construye el nodo inicial para comenzar el Backtracking
initialNode :: Map -> Node
initialNode m = (adjacencies m, numZonas m, firstZona m, firstSolPar (numZonas m))
  where
    -- Devuelve el número de zonas de un mapa
    numZonas :: Map -> Zone
    numZonas m = numZonasAux1 m 0
      where
        numZonasAux1 :: Map -> Zone -> Zone
        numZonasAux1 (r:m) x = numZonasAux1 m (numZonasAux2 r x)
          where
            numZonasAux2 :: Row -> Zone -> Zone
            numZonasAux2 (z:r) x
              | z > x     = numZonasAux2 r z
              | otherwise = numZonasAux2 r x
            numZonasAux2 [] x = x
        numZonasAux1 [] x = x
    -- Devuelve la primera zona del mapa 
    firstZona :: Map -> Zone
    firstZona ((z:zs):rs) = z
    firstZona _ = 0
    -- Devuelve la primera solución parcial
    firstSolPar :: Zone -> Solution
    firstSolPar x
      | x == 0    = []
      | otherwise = Red : firstSolPar (x-1)