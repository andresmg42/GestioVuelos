import Itinerarios._
import ItinerariosPar._
import Datos._
import org.scalameter._
import java.io._
package object Benchmark {
  type AlgoritmIt= (List[Vuelo],List[Aeropuerto]) =>(String,String)=> List[Itinerario]
  type AlgoritmIt2=(String,String)=>List[Itinerario]
  type AlgoritmItS=(List[Vuelo],List[Aeropuerto])=>(String,String,Int,Int)=>Itinerario
  type AlgoritmItS2=(String,String,Int,Int)=>Itinerario

// compara algoritmos pasandole los vuelos, aeropuertos y los codigo de origen y destino, no funciona para itinerarioSalida.
  def compararAlgoritmos(a1:AlgoritmIt, a2:AlgoritmIt)(vuelos:List[Vuelo],aeropuerto:List[Aeropuerto])(cod1:String,cod2:String):(Double,Double, Double) = {
    val timeA1 = config(
      KeyValue(Key.exec.minWarmupRuns -> 20),
      KeyValue(Key.exec.maxWarmupRuns -> 60),
      KeyValue(Key.verbose -> false)
    ) withWarmer(new Warmer.Default) measure (a1(vuelos,aeropuerto)(cod1,cod2))

    val timeA2 = config(
      KeyValue(Key.exec.minWarmupRuns -> 20),
      KeyValue(Key.exec.maxWarmupRuns -> 60),
      KeyValue(Key.verbose -> false)
    ) withWarmer(new Warmer.Default) measure (a2(vuelos,aeropuerto)(cod1,cod2))

    val speedUp= timeA1.value/timeA2.value
    (timeA1.value, timeA2.value, speedUp)
  }

//compara algoritmos pasandole solo los codigos de origen y destino, no compara algoritmos itinerarioSalida
 def compararAlgoritmos2(a1:AlgoritmIt2, a2:AlgoritmIt2)(cod1:String,cod2:String):(Double,Double, Double) = {
    val timeA1 = config(
      KeyValue(Key.exec.minWarmupRuns -> 20),
      KeyValue(Key.exec.maxWarmupRuns -> 60),
      KeyValue(Key.verbose -> false)
    ) withWarmer(new Warmer.Default) measure (a1(cod1,cod2))

    val timeA2 = config(
      KeyValue(Key.exec.minWarmupRuns -> 20),
      KeyValue(Key.exec.maxWarmupRuns -> 60),
      KeyValue(Key.verbose -> false)
    ) withWarmer(new Warmer.Default) measure (a2(cod1,cod2))

    val speedUp= timeA1.value/timeA2.value
    (timeA1.value, timeA2.value, speedUp)
  }
//compara algoritmos itinerarioSalida
 def compararAlgoritmoSalida(a1:AlgoritmItS2, a2:AlgoritmItS2)(cod1:String,cod2:String,H:Int,M:Int):(Double,Double, Double) = {
    val timeA1 = config(
      KeyValue(Key.exec.minWarmupRuns -> 20),
      KeyValue(Key.exec.maxWarmupRuns -> 60),
      KeyValue(Key.verbose -> false)
    ) withWarmer(new Warmer.Default) measure (a1(cod1,cod2,H,M))

    val timeA2 = config(
      KeyValue(Key.exec.minWarmupRuns -> 20),
      KeyValue(Key.exec.maxWarmupRuns -> 60),
      KeyValue(Key.verbose -> false)
    ) withWarmer(new Warmer.Default) measure (a2(cod1,cod2,H,M))

    val speedUp= timeA1.value/timeA2.value
    (timeA1.value, timeA2.value, speedUp)
  }



//crea la pruebas para todos los algoritmos a exepcion de itinerariosSalida.
def crearPruebas(
    vuelosPrueba: Map[List[Vuelo], (String, String)],
    aeropuertos: List[Aeropuerto],
    numPruevas: Int,
    algoritmos: (AlgoritmIt, AlgoritmIt)
): List[List[((Double, Double, Double), Int)]] = {
  val res = for {
    (v, codA) <- vuelosPrueba
    a1 = algoritmos._1(v, aeropuertos)
    a2 = algoritmos._2(v, aeropuertos)

  } yield (for {
    rep <- 1 to numPruevas
  } yield (compararAlgoritmos2(a1, a2)(codA._1, codA._2), v.length)).toList
  res.toList
}

//crear pruebas donde se recorra toda la estructura Map sin Itierar por cada Vuelo, no funciona para itinerarioSalida.
def crearPruebasSinCiclos(
    vuelosPrueba: Map[List[Vuelo], (String, String)],
    aeropuertos: List[Aeropuerto],
    numPruevas: Int,
    algoritmos: (AlgoritmIt, AlgoritmIt)
): List[List[((Double, Double, Double), Int)]] = {
   val res=for {
    (v, codA) <- vuelosPrueba
    a1 = algoritmos._1(v, aeropuertos)
    a2 = algoritmos._2(v, aeropuertos)

  } yield List((compararAlgoritmos2(a1, a2)(codA._1, codA._2),v.length))
  res.toList

}

// crea las pruebas para itinerariosSalida
def crearPruebaSalida(
    vuelosPrueba: Map[List[Vuelo], (String, String)],
    aeropuertos: List[Aeropuerto],
    numPruevas: Int,
    H:Int,
    M:Int,
    algoritmos: (AlgoritmItS, AlgoritmItS)
): List[List[((Double, Double, Double), Int)]] = {
  val res = for {
    (v, codA) <- vuelosPrueba
    a1 = algoritmos._1(v, aeropuertos)
    a2 = algoritmos._2(v, aeropuertos)

  } yield (for {
    rep <- 1 to numPruevas
  } yield (compararAlgoritmoSalida(a1, a2)(codA._1, codA._2,H,M), v.length)).toList
  res.toList
}



// convierte las pruebas en un archivo .csv
def escribirCsv(
    rutacsv: String,
    datos: List[List[((Double, Double, Double), Int)]]
): Unit = {

  val writer = new PrintWriter(new File(rutacsv))
  try {
    datos.foreach { listaExterna =>
      listaExterna.foreach { case ((a, b, c), d) =>
        writer.println(s"$d,$a,$b,$c")
      }

    }

  } finally {
    writer.close()

  }

}


  
}
