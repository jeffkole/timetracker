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

  test("Category duration accumulator") {
    val day_1 = new Day(format.parse("2011/01/04"))
    var durations = List(20, 10, 5, 30, 5) // min: 5, max: 30, avg: 14
    val day_1_total = durations.foldLeft(0)(_ + _)
    durations.foreach { duration => day_1.addTask(new Task(day_1, duration, "category", null)) }

    val day_2 = new Day(format.parse("2011/01/05"))
    durations = List(5, 15)
    val day_2_total = durations.foldLeft(0)(_ + _)
    durations.foreach { duration => day_2.addTask(new Task(day_2, duration, "category", null)) }

    expect(day_1_total, "Day 1 category durations wrong")(day_1.categoryDurations.get("category").get)
    expect(day_2_total, "Day 2 category durations wrong")(day_2.categoryDurations.get("category").get)
    var categoryDurations = day_2.categoryDurations(day_1.categoryDurations)
    expect(day_1_total + day_2_total, "Combined total wrong")(categoryDurations.get("category").get)

    day_1.addTask(new Task(day_1, 10, "uno", null))
    day_2.addTask(new Task(day_2, 14, "quatro", null))
    categoryDurations = day_2.categoryDurations(day_1.categoryDurations)
    expect(day_1_total + day_2_total, "Combined total wrong")(categoryDurations.get("category").get)
    expect(10, "Extra day 1 category wrong")(categoryDurations.get("uno").get)
    expect(14, "Extra day 2 category wrong")(categoryDurations.get("quatro").get)
  }
}
