package models

import java.util.UUID

import ru.yudnikov.core.{ Manager, Model, Reference }
import models.enums.Currency

/**
 * Created by Don on 11.06.2017.
 */
class Lot(
  val bookmark: Reference[Bookmark],
  val price: Int,
  val list: List[Reference[Product]],
  val currency: Currency.Value = Currency.RUR,
  val id: UUID = UUID.randomUUID()
) extends Model(Lot)

object Lot extends Manager[Lot] {

  override def update(model: Model): Unit = {
    val lot = model.asInstanceOf[Lot]
    val lots = Lot.list(l => l != lot && l.bookmark.id == lot.bookmark.id)
    if (lots.isEmpty)
      super.update(model)
    else
      println(s"bookmark is also used in $lots")
  }

}
