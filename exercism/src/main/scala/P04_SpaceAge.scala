object P04_SpaceAge {
  def onEarth(age: Double): Double = age / 31557600
  def onVenus(age: Double): Double = age / (31557600 * 0.61519726)
  def onMercury(age: Double): Double = age / (31557600 * 0.2408467)
  def onMars(age: Double): Double = age / (31557600 * 1.8808158)
  def onJupiter(age: Double): Double = age / (31557600 * 11.862615)
  def onSaturn(age: Double): Double = age / (31557600 * 29.447498)
  def onUranus(age: Double): Double = age / (31557600 * 84.016846)
  def onNeptune(age: Double): Double = age / (31557600 * 164.79132)
}