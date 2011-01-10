package com.kolesky.timetracker

import com.kolesky.timetracker._
import com.kolesky.timetracker.model._

import java.text.SimpleDateFormat
import java.util.Date

import scala.collection.mutable.ArrayBuffer

object Main {
  def main(args: Array[String]) {
    assert(args.length > 0)
    val filename = args(0)

    val format = new SimpleDateFormat("yyyy/MM/dd")
    val days = new ArrayBuffer[Day]
    var day: Day = null
    var date: Date = null
    io.Source.fromFile(filename).getLines.foreach { line =>
      val fields = line.split('\t')
      // skip the header
      // LERNIN: there must be a better way to do this?
      if (fields(0) != "Date") {
        val taskDate = if (fields(0).nonEmpty) { format.parse(fields(0)) } else { date }
        // if we are on a new day, then finish up the previous one
        if (taskDate != date) {
          day = new Day(taskDate)
          days += day
          date = taskDate
        }
        day.addTask(new Task(day, fields(3).toInt, fields(4), fields(5)))
      }
    }

    printf("%10s  %7s  %9s  %4s  %4s  %4s  %n", "Date", "# Tasks", "# Repeats",  "Min", "Avg", "Max");
    days.foreach { day =>
      printf("%1$tm-%1$td-%1$tY  %2$7d  %3$9d  %4$4s  %5$4s  %6$4s  %n", 
             day.date, day.numTasks, day.numRepeatTasks, day.minDuration, day.avgDuration, day.maxDuration)
    }
  }
}
