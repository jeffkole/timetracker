package com.kolesky.timetracker.model

import java.util.Calendar
import java.util.Calendar._
import java.util.Date

import scala.collection.mutable.HashMap

class Day(val date: Date) {
  val week = {
    val cal = Calendar.getInstance();
    cal.setTime(date)
    cal.get(WEEK_OF_YEAR)
  }
  
  // LERNIN: had to assign a default value for the private List
  // otherwise, I got this compile error: abstract member may not have private modifier
  private var tasks: List[Task] = List()

  // LERNIN: decided that for the sake of self-documenting, always specifying a return type is a good thing
  def addTask(task: Task): Unit = {
    tasks = task :: tasks
  }

  def numTasks: Int = {
    tasks.size
  }

  def numRepeatTasks: Int = {
    tasks.filter(_.repeat).size
  }

  def numUnnecessaryTasks: Int = {
    tasks.filter(_.unnecessary).size
  }

  /**
   * @return the minimum duration of all tasks
   */
  def minDuration: Int = {
    tasks.map(_.duration).min
  }

  /**
   * @return the maximum duration of all tasks
   */
  def maxDuration: Int = {
    tasks.map(_.duration).max
  }

  /**
   * @return the average (arithmetic mean) of the duration of all tasks
   */
  def avgDuration: Int = {
    // LERNIN: need to pass in the start value of 0
    tasks.foldLeft(0)(_ + _.duration) / tasks.size
  }

  /**
   * @return a map of category names to total duration
   */
  def categoryDurations: Map[String, Int] = {
    // LERNIN: when I forgot to add the type parameter to the new HashMap declaration, I got
    // this compilation error: 
    // error: type mismatch;
    // found   : task.category.type (with underlying type String)
    // required: Nothing
    //      val duration = categories.get(task.category).getOrElse(0)
    //                                         ^
    val categories = new HashMap[String, Int]
    tasks.foreach { task =>
      val duration = categories.get(task.category).getOrElse(0)
      categories.put(task.category, duration + task.duration)
    }
    categories.toMap
  }

  /**
   * Accumulates values in the given Map with category durations in this Day's tasks by summing
   * the durations.
   * LERNIN: a cool trick would be to have this method take an implicit parameter that defaults
   * to a "duration sum" function that could be overloaded with another accumulator function.
   *
   * @return a map with the new accumulated values
   */
  def categoryDurations(categoryDurations: Map[String, Int]): Map[String, Int] = {
    // LERNIN: this results in a deprecation warning, but I can't figure out why and
    // am not going to take the time to figure out how to have buildr give me the warning details
    val categories = new HashMap[String, Int] ++ categoryDurations
    // LERNIN: is this structure better than tasks.foreach as used in categoryDurations?
    for (val task <- tasks) {
      val duration = categories.get(task.category).getOrElse(0)
      categories.put(task.category, duration + task.duration)
    }
    categories.toMap
  }

  override def toString = {
    format("Day: %tD%n%s", date, tasks.map(_.toString + "\n"))
  }
}

object Day {
  // LERNIN: change this to take a PartialFunction that pulls out the correct field of Day
  def medianNumTasks(days: List[Day]): Double = {
    // LERNIN: There has to be a way to use an implicit so that I don't have to convert
    // an Int to itself
    val sorted = days.map(_.numTasks).sortBy((a) => a)
    if (sorted.size % 2 == 0) {
      val idx = sorted.size / 2
      val high = sorted(idx)
      val low  = sorted(idx - 1)
      return ((high + low) / 2.0)
    }
    sorted(sorted.size / 2)
  }
}
