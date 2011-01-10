package com.kolesky.timetracker.model

import org.scalatest.FunSuite

class TastTest extends FunSuite {
  test("Category parsing") {
    var task = new Task(null, 1, "category", "description")
    assert(task.category == "category")
    task = new Task(null, 1, "repeat*", "description")
    assert(task.category == "repeat")
  }
}
