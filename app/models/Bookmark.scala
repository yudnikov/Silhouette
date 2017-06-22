package models

import java.util.UUID

import ru.yudnikov.core.{ Manager, Model, Reference }
import org.joda.time.DateTime

/**
 * Created by Don on 11.06.2017.
 */
class Bookmark(
  val product: Reference[Product],
  val place: Reference[Place],
  val amount: Int,
  val list: List[Any],
  val date: DateTime = new DateTime(),
  val id: UUID = UUID.randomUUID()
) extends Model(Bookmark) {

}

object Bookmark extends Manager[Bookmark] {

  def samePlace(bookmark: Bookmark): List[Bookmark] = list(_.place == bookmark.place)

  override def update(model: Model): Unit = {
    val bookmark = model.asInstanceOf[Bookmark]
    if (Place.bookmarkAllowed(bookmark))
      super.update(model)
    else
      println(s"$bookmark cannot be done @ ${bookmark.place}!")
  }

}
