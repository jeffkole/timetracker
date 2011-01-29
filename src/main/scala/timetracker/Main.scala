package com.kolesky.timetracker

import com.kolesky.timetracker._
import com.kolesky.timetracker.model._

import java.text.SimpleDateFormat
import java.util.Date

object Main {
  private val HISTOGRAM_MAX: Int = 80

  def main(args: Array[String]) {
    assert(args.length > 0)
    val filename = args(0)

    val format = new SimpleDateFormat("yyyy/MM/dd")
    var days = List.empty[Day]
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
          days = day :: days
          date = taskDate
        }
        day.addTask(new Task(day, fields(3).toInt, fields(4), fields(5)))
      }
    }

    // LERNIN: If you want an empty immutable map, you have to ask for one explicitly
    // and not just expect that you can instantiate one with 'new'
    var categoryDurations = Map.empty[String, Int]
    printf("%10s  %7s  %9s  %7s  %7s  %7s  %7s  %n",
           "Date", "# Tasks", "# Repeats", "# Unnec", "Min", "Avg", "Max");
    for (val day <- days) {
      printf("%1$tm-%1$td-%1$tY  %2$7d  %3$9d  %4$7d  %5$7s  %6$7s  %7$7s  %n",
             day.date, day.numTasks, day.numRepeatTasks, day.numUnnecessaryTasks,
             day.minDuration, day.avgDuration, day.maxDuration)
      // Accumulate category durations for all days
      categoryDurations = day.categoryDurations(categoryDurations)
    }
    printf("%s%n", "-" * (10 + 5 + 7 + 2 + 9 + 2 + 7 + 2 + 7 + 2 + 7 + 2 + 7 + 2))
    printf("%10s     %7.2f  %9.2f  %7.2f  %7.2f  %7.2f  %7.2f  %n", " ",
           Day.medianNumTasks(days), Day.medianNumRepeats(days), Day.medianNumUnnecessary(days),
           Day.medianMinDuration(days), Day.medianAvgDuration(days), Day.medianMaxDuration(days))
    printf("%n")

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

    // LERNIN: toList returns a list of Tuple2[String, Int], and we want to sort by the Int
    // in position 2, thus the _2.
    val sortedDurations = categoryDurations.toList.sortBy((a) => a._2).reverse
    for ((category, duration) <- sortedDurations) {
      val histogram: Int = (duration * HISTOGRAM_MAX) / maxDuration
      val percent: Double = (duration / totalDuration) * 100
      printf("  %8s (%4d, %3.0f%%) %s%n", category, duration, percent, "*" * histogram)
    }
  }
}
