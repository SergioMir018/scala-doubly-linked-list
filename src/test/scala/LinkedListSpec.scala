import org.scalatest.flatspec.AnyFlatSpec
import org.scalatest.matchers.should.Matchers.*

class LinkedListSpec extends AnyFlatSpec {
  "A Linked_List" should "be able to prepend elements" in {
    val list = new Linked_List[Int]()
    list.prepend(5)
    list.prepend(10)
    list.prepend(15)

    list.getAt(0) shouldBe Some(15)
    list.getAt(1) shouldBe Some(10)
    list.getAt(2) shouldBe Some(5)
  }

  it should "be able to append elements" in {
    val list = new Linked_List[Int]()
    list.append(5)
    list.append(10)
    list.append(15)

    list.getAt(0) shouldBe Some(5)
    list.getAt(1) shouldBe Some(10)
    list.getAt(2) shouldBe Some(15)
  }

  it should "be able to get elements at a specific index" in {
    val list = new Linked_List[String]()
    list.append("apple")
    list.append("banana")
    list.append("orange")

    list.getAt(0) shouldBe Some("apple")
    list.getAt(1) shouldBe Some("banana")
    list.getAt(2) shouldBe Some("orange")
  }

  it should "be able to remove elements" in {
    val list = new Linked_List[Int]()
    list.append(5)
    list.append(10)
    list.append(15)
    list.append(20)

    list.remove(10)
    list.getAt(1) shouldBe Some(15)
    list.remove(5)
    list.getAt(0) shouldBe Some(15)
  }
}
