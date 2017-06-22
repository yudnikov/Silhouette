package models

import java.util.UUID

import ru.yudnikov.core.{ Manager, Model }
import models.enums.ProductCategory

/**
 * Created by Don on 11.06.2017.
 */
class Product(
  val name: String,
  val description: String,
  val productCategory: ProductCategory.Value = ProductCategory.Others,
  val id: UUID = UUID.randomUUID()
) extends Model(Product) {

}

object Product extends Manager[Product] {

}
