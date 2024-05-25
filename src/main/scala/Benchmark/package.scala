import Itinerarios._
import ItinerariosPar._
import Datos._
import org.scalameter._
package object Benchmark {
  type AlgoritmIt= (List[Vuelo],List[Aeropuerto]) =>(String,String)=> List[Itinerario]
  type AlgoritmIt2=(String,String)=>List[Itinerario]


  /*def compararAlgoritmos(a1:AlgoritmIt, a2:AlgoritmIt)(vuelos:List[Vuelo],aeropuerto:List[Aeropuerto])(cod1:String,cod2:String):(Double,Double, Double) = {
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
  }*/


  def compararAlgoritmos3(a1:AlgoritmIt2, a2:AlgoritmIt2)(cod1:String,cod2:String):(Double,Double, Double) = {
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



}