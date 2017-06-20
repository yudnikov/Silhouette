package modules

import com.google.inject.AbstractModule
import net.codingwell.scalaguice.ScalaModule
import ru.yudnikov.core.postman.{ Postman, SuperPostman }

/**
 * Created by Don on 16.06.2017.
 */
class MyModule extends AbstractModule with ScalaModule {
  override def configure(): Unit = {
    bind(classOf[Postman]).to(classOf[SuperPostman])
  }
}
