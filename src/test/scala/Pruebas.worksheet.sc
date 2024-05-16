case class Aeropuerto(cod: String, x: Int, y: Int, GMT: Int)

case class Vuelo(
    Aln: String,
    Num: Int,
    Org: String,
    HS: Int,
    MS: Int,
    Dst: String,
    HL: Int,
    ML: Int,
    Esc: Int
)

type Itinerario = List[Vuelo]

val aeropuertosCurso = List(
  Aeropuerto("CLO", 100, 200, -500), // Cali
  Aeropuerto("BOG", 300, 500, -500), // Bogotá
  Aeropuerto("MDE", 200, 600, -500), // Medellin
  Aeropuerto("BAQ", 350, 850, -500), // Barranquilla
  Aeropuerto("SMR", 400, 950, -500), // Santa Marta
  Aeropuerto("CTG", 300, 800, -500), // Cartagena
  Aeropuerto("PTY", 400, 1000, -500), // Ciudad de Panamá
  Aeropuerto("JFK", 2000, 2000, -400), // Nueva York
  Aeropuerto("MIA", 1000, 2000, -500), // Miami
  Aeropuerto("MEX", 1000, 1000, -600), // Ciudad de México
  Aeropuerto("MAD", 5000, 5000, 100), // Madrid
  Aeropuerto("SVCS", 400, 1000, -600), // Caracas
  Aeropuerto("MID", 500, 100, -600), // Merida
  Aeropuerto("AUA", 500, 2000, -400), // Aruba
  Aeropuerto("IST", 9000, 9000, 300), // Estambul
  Aeropuerto("HND", 10000, 12000, 900), // Tokio
  Aeropuerto("DXB", 9500, 11500, 400), // Dubai
  Aeropuerto("SVO", 12500, 12500, 300) // Moscú
)

val vuelosCurso = List(
  Vuelo("AIRVZLA", 601, "MID", 5, 0, "SVCS", 6, 0, 0),
  Vuelo("AIRVZLA", 602, "SVCS", 6, 30, "MID", 7, 30, 0),
  Vuelo("AVA", 9432, "CLO", 7, 0, "SVO", 2, 20, 4),
  Vuelo("AVA", 9432, "CLO", 7, 0, "BOG", 8, 0, 0),
  Vuelo("IBERIA", 505, "BOG", 18, 0, "MAD", 12, 0, 0),
  Vuelo("IBERIA", 506, "MAD", 14, 0, "SVO", 23, 20, 0),
  Vuelo("IBERIA", 507, "MAD", 16, 0, "SVO", 1, 20, 0),
  Vuelo("LATAM", 787, "BOG", 17, 0, "MEX", 19, 0, 0),
  Vuelo("VIVA", 756, "BOG", 9, 0, "MDE", 10, 0, 0),
  Vuelo("VIVA", 769, "MDE", 11, 0, "BAQ", 12, 0, 0),
  Vuelo("AVA", 5643, "BAQ", 14, 0, "MEX", 16, 0, 0),
  Vuelo("COPA", 1234, "CTG", 10, 0, "PTY", 11, 30, 0),
  Vuelo("AVA", 4321, "CTG", 9, 30, "SMR", 10, 0, 0),
  Vuelo("COPA", 7631, "SMR", 10, 50, "PTY", 11, 50, 0),
  Vuelo("TURKISH", 7799, "CLO", 7, 0, "IST", 14, 0, 3),
  Vuelo("QATAR", 5566, "IST", 23, 0, "SVO", 2, 0, 0)
)

//Itinerarios normal--------------------------------------------------------------------------------------------------------------------
    """
    Esta función retorna todos los itinerarios que salen de cod1 hasta cod2

    Args:
        vuelos(List[Vuelo]: lista de objetos de tipo Vuelo
        aeropuertos(List[Aeropuerto]): lista de objetos de tipo Aeropuerto

    Returns:
        List[Itinerario]=List(List[Vuelo]): una lista  de Itinerarios de los vuelos que parten de cod1 hasta cod2   """


def itinerarios(vuelos:List[Vuelo],aeropuertos:List[Aeropuerto]):(String,String)=>List[Itinerario]={
val aeropuertosMap = aeropuertos.map(airport => airport.cod -> airport).toMap //retorna una estructura tipo Map tal que la llave sea el codigo del aeropuerto y el valor el objeto Aeropuerto
  def formarItinerarios(cod1:String,cod2:String,visitados:Set[String]):List[Itinerario]={//funcion auxiliar que resive los codigos de los aeropuertos y un conjunto(Visitados:Set[String]) donde se almacenan los codigos de los aeropuertos visitados sin repertirse.
    if(cod1==cod2) List(Nil)// caso base, la funcion se detiene cuando el aeropuerto de origen es el aeropuerto de destino y devuelve una lista con una lista vacia(Nil).
    else{
      val vuelosDesdeCod1=vuelos.filter(_.Org==cod1) // filtramos todos los vuelos cuyo origen sea cod1 y los guardamos en vuelosDesdeCod1
      for{
        v<-vuelosDesdeCod1//sacamos cada vuelo v de vuelosDesdeCod1
        if!visitados(v.Dst)// si el vuelo siguiente a cada vuelo v, es desir el destino de v(v.Dst) hacemos:
        itRestante<-formarItinerarios(v.Dst,cod2,visitados + v.Dst)// aplicacamos recursivamente formarItinerarios a cada vuelo v de vuelosDesdeCod1 siendo ahora cod1 el codigo del aeropuerto destino del vuelo anterior y adicionamos
                                                                   //el vuelo v presente al conjunto de visitados ya que no se puede visitar un aeropuerto dos veces. cod2 pasa igual ya que es el codigo del aeropuerto de destino. Posterior mente desempaquetamos cada itinerario en itRestante
     } yield v::itRestante                                         // y le adicionamos el vuelo v presente a la caveza del itRestante ya que es el vuelo de origen que no se incluyo en formarItinerarios(v.Dst,cod2,visitados + v.Dst).
               
  
    
    }
  }

  (cod1: String, cod2: String) => {             //finalmente creamos una funcion anonima que resive cod1 y cod2 
    val aeropuerto1 = aeropuertosMap.get(cod1) //luego se extraen los objetos aeropuerto1 y aeropuerto2, de la estructura Map con .get(cod1) y  .get(cod2)
    val aeropuerto2 = aeropuertosMap.get(cod2) 
    (aeropuerto1, aeropuerto2) match {  //utilizamos reconosimiento de patrones son Some(aeropuerto1) y some(aeropurto2) los cuales si existe un aeropuerto con ese codigo devuelven algo, sino devuelven null
      case (Some(airport1), Some(airport2)) =>
        formarItinerarios(cod1, cod2, Set(cod1)) // en caso de que existan esos aeropuertos se invoca a la funcion formarItinerarios(cod1,cod2,set(cod1)) donde set(cod1) es el conjunto de los aeropuertos visitados, se coloca cod1 ya que es el aeropuerto de origen.
      case _ => Nil // Si alguno de los aeropuertos no existe, devolver una lista vacía
    }
  }
}

//Itinerarios Inverso---------------------------------------------------------------------------------------------------------------
def itinerariosInversos(vuelos: List[Vuelo], aeropuertos: List[Aeropuerto]): (String, String) => List[Itinerario] = {
  val aeropuertosMap = aeropuertos.map(airport => airport.cod -> airport).toMap

  def formarItinerarios(cod1: String, cod2: String, visitados: Set[String]): List[Itinerario] = {
    if (cod1 == cod2)
      List(Nil)
    else {
      val vuelosHastaCod2 = vuelos.filter(_.Dst == cod2)

      for{
        v<-vuelosHastaCod2
        if!visitados(v.Org)
        itRestante<-formarItinerarios(cod1,v.Org,visitados + v.Org)
      }yield itRestante:+v

    }
  }

  (cod1: String, cod2: String) => {
    val aeropuerto1 = aeropuertosMap.get(cod1)
    val aeropuerto2 = aeropuertosMap.get(cod2)
    (aeropuerto1, aeropuerto2) match {
      case (Some(airport1), Some(airport2)) =>
        formarItinerarios(cod1, cod2, Set(cod2)) // Empezamos desde el aeropuerto de destino (cod2)
      case _ => Nil // Si alguno de los aeropuertos no existe, devolver una lista vacía
    }
  }
} 