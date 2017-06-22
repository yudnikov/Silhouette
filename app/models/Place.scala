package models

import java.util.UUID

import ru.yudnikov.core.{ Manager, Model, Reference }
import models.settings.Settings

/**
 * Created by Don on 11.06.2017.
 */
class Place(
  val latitude: Double,
  val longitude: Double,
  val description: String,
  val id: UUID = UUID.randomUUID()
) extends Model(Place) {

}

object Place extends Manager[Place] {

  private var bookmarks: Map[Reference[Place], List[Bookmark]] = Map()

  def bookmarkAllowed(bookmark: Bookmark): Boolean = {
    val placeBookmarks = bookmarks.get(bookmark.place)
    if (placeBookmarks.isDefined &&
      (placeBookmarks.get.isEmpty
        || placeBookmarks.get.contains(bookmark)
        || placeBookmarks.get.head.date.plusDays(Settings.PLACE_COOL_DOWN_DAYS).isBefore(bookmark.date))) {
      bookmarks = bookmarks + (bookmark.place -> (bookmark :: placeBookmarks.get))
      true
    } else false
  }

  override def update(model: Model): Unit = {
    val place = model.asInstanceOf[Place]
    if (near(place).isEmpty) {
      bookmarks = bookmarks + (place.reference -> Nil)
      super.update(model)
    } else
      println(s"$place is too close to other places!")
  }

  def near(place: Place): List[Place] = {
    val f: (Place) => Boolean = p =>
      p != place &&
        Math.abs(p.latitude - place.latitude) < Settings.PLACE_CLOSENESS_MEASURE &&
        Math.abs(p.longitude - place.longitude) < Settings.PLACE_CLOSENESS_MEASURE
    list(f)
  }
}
