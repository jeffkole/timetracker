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

  def addTask(task: Task) = {
    tasks = task :: tasks
  }

  def numTasks = {
    tasks.size
  }

  def numRepeatTasks = {
    tasks.filter(_.repeat).size
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

  override def toString = {
    format("Day: %tD%n%s", date, tasks.map(_.toString + "\n"))
  }
}