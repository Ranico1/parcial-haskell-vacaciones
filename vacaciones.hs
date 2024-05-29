data Turista = Turista {
nivelDeStress :: Int,
nivelDeCansancio :: Int,
conAcompanante :: Bool, 
idiomas :: [Idioma]
}

type Idioma = String  


-- TURISTAS MODELADOS 

ana = Turista {
nivelDeStress = 21,
nivelDeCansancio = 0,
conAcompanante = True,
idiomas = ["espanol"]
}

beto = Turista {
nivelDeStress = 15,
nivelDeCansancio = 15,
conAcompanante = True,
idiomas = ["aleman"] 

cathi = Turista {
nivelDeStress = 15,
nivelDeCansancio = 15,
conAcompanante = True,
idiomas = ["aleman", "catalan"]

}
}


-- PUNTO 1

irAlaPlaya :: Turista -> Turista
irAlaPlaya unTurista  
    | conAcompanante unTurista = mapCansancio (subtract 5) unTurista
    | otherwise = mapStress (subtract 1) unTurista

mapStress :: (Int -> Int) -> Turista -> Turista 
mapStress f unTurista = unTurista {nivelDeStress = f (nivelDeStress unTurista)}    

mapCansancio :: (Int -> Int) -> Turista -> Turista 
mapCansancio f unTurista = unTurista {nivelDeCansancio = f (nivelDeCansancio unTurista)}

apreciarPaisaje :: Elemento-> Turista -> Turista
apreciarPaisaje unElemento = mapStress (subtract (length unElemento)) 

hablarIdiomas :: Idioma -> Turista -> Turista
hablarIdiomas unIdioma  = viajaAcompanado.mapIdiomas (unIdioma:) 

mapIdiomas :: ([Idioma] -> [Idioma]) -> Turista -> Turista
mapIdiomas f unTurista = unTurista {idiomas = f (idiomas unTurista)} 

viajaAcompanado :: Turista -> Turista
viajaAcompanado unTurista = unTurista {conAcompanante = True}

hacerCaminata :: Int -> Turista -> Turista
hacerCaminata minutos = (mapCansancio (+ (intensidadCaminata minutos)) . mapStress (subtract  (intensidadCaminata minutos))) 


intensidadCaminata :: Int -> Int 
intensidadCaminata minutos = div minutos 4  

data Marea =  Fuerte | Moderada | Tranquila (deriving Eq)

estadoDeMarea :: Marea -> Turista -> Turista
estadoDeMarea unaMarea unTurista
    | unaMarea == Fuerte = (mapCansancio (+ 10). mapStress (+6)) unTurista
    | unaMarea == Tranquila = (hablarIdiomas "aleman".apreciarPaisaje "mar".hacerCaminata 10 ) unTurista 
    | otherwise = unTurista

type Elemento = String


--PUNTO 2
type Excursion = Turista -> Turista


hacerExcursion :: Excursion -> Turista -> Turista
hacerExcursion unaExcursion unTurista = (mapStress (subtract porcentageDeStress 10).unaExcursion) $ unTurista

porcentageDeStress :: Turista -> Int -> Int 
porcentageDeStress porcentage = div ( porcentage * nivelDeStress) 100  


-- PUNTO 2B 

deltaSegun :: (a -> Int) -> a -> a -> Int
deltaSegun f algo1 algo2 = f algo1 - f algo2

deltaExcursionSegun :: (a -> Int) -> a -> Excursion -> Int
deltaExcursionSegun indice unTurista unaExcursion = deltaSegun indice unTurista (hacerExcursion unaExcursion unTurista)