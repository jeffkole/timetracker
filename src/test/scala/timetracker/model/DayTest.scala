package com.kolesky.timetracker.model

import java.text.SimpleDateFormat
import java.util.Calendar
import java.util.Date

import org.scalatest.FunSuite

class DayTest extends FunSuite {
  val format = new SimpleDateFormat("yyyy/MM/dd")

  test("Week calculation") {
    var day = new Day(format.parse("2011/01/01"))
    assert(day.week == 1)
    day = new Day(format.parse("2011/01/04"))
    assert(day.week == 2)
  }

  test("Add task increments the task size") {
    var day = new Day(format.parse("2011/01/04"))
    assert(day.numTasks == 0)
    day.addTask(new Task(day, 1, "category", null))
    assert(day.numTasks == 1)
  }

  test("Min/max/average durations") {
    val day = new Day(format.parse("2011/01/04"))
    val durations = List(20, 10, 5, 30, 5) // min: 5, max: 30, avg: 14
    durations.foreach { duration => day.addTask(new Task(day, duration, "category", null)) }
    assert(day.minDuration == 5)
    assert(day.maxDuration == 30)
    assert(day.avgDuration == 14)
    day.addTask(new Task(day, 4, "category", null))
    assert(day.avgDuration == 12)
  }

  test("Category durations") {
    val day = new Day(format.parse("2011/01/04"))
    val durations = List(20, 10, 5, 30, 5) // min: 5, max: 30, avg: 14
    durations.foreach { duration => day.addTask(new Task(day, duration, "category", null)) }
    expect(durations.foldLeft(0)(_ + _), "Category durations wrong")(day.categoryDurations.get("category").get)
    day.addTask(new Task(day, 4, "category", null))
    expect(durations.foldLeft(0)(_ + _) + 4, "Category durations wrong second time")(day.categoryDurations.get("category").get)
  }
}
