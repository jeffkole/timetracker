package com.kolesky.timetracker

import com.kolesky.timetracker._
import com.kolesky.timetracker.model._

import java.text.SimpleDateFormat
import java.util.Date

import scala.collection.mutable.ArrayBuffer

object Main {
  private val HISTOGRAM_MAX: Int = 80

  def main(args: Array[String]) {
    assert(args.length > 0)
    val filename = args(0)

    val format = new SimpleDateFormat("yyyy/MM/dd")
    val days = new ArrayBuffer[Day]
    var day: Day = null
    var date: Date = null
    for (val line <- io.Source.fromFile(filename).getLines) {
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

    // LERNIN: If you want an empty immutable map, you have to ask for one explicitly
    // and not just expect that you can instantiate one with 'new'
    var categoryDurations = Map.empty[String, Int]
    printf("%10s  %7s  %9s  %4s  %4s  %4s  %n", "Date", "# Tasks", "# Repeats",  "Min", "Avg", "Max");
    for (val day <- days) {
      printf("%1$tm-%1$td-%1$tY  %2$7d  %3$9d  %4$4s  %5$4s  %6$4s  %n", 
             day.date, day.numTasks, day.numRepeatTasks, day.minDuration, day.avgDuration, day.maxDuration)
      // Accumulate category durations for all days
      categoryDurations = day.categoryDurations(categoryDurations)
    }
    printf("%n");

    // Find the max value of all durations over all days
    // LERNIN: figure out how to do this idiomatically
    var maxDuration = 0
    var totalDuration = 0.0 // Make this a double so we don't lose precision later
    for ((category, duration) <- categoryDurations) {
      if (duration > maxDuration) {
        maxDuration = duration
      }
      totalDuration += duration
    }

    for ((category, duration) <- categoryDurations) {
      val histogram: Int = (duration * HISTOGRAM_MAX) / maxDuration
      val percent: Double = (duration / totalDuration) * 100
      printf("  %8s (%4d, %3.0f%%) %s%n", category, duration, percent, "*" * histogram)
    }
  }
}
