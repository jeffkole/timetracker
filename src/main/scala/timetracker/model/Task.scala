package com.kolesky.timetracker.model

// LERNIN: Should this be a case class?
// LERNIN: If rawCategory is declared as a val here, is it exposed as a property of Task, and if so
// how can I prevent it from being read by the outside world?  It should never be referenced once
// the Task has been constructed.
// LERNIN: leaving off 'val' makes rawCategory private
class Task(val day: Day, val duration: Int, rawCategory: String, val description: String) {
  val (category, repeat, unnecessary) = Task.parseCategory(rawCategory)

  override def toString = {
    format("   Task: %s; %s; %s", duration, category, description)
  }
}

object Task {
  // TODO: This is a poor design.  This parsing logic should live elsewhere and a Task should just
  // take the final raw values.
  private val tagRegex = "([^*-]+)([*-]+)$".r
  private def parseCategory(category: String): (String, Boolean, Boolean) = {
    tagRegex.findFirstMatchIn(category) match {
      case Some(groups) => {
        return (groups.group(1), groups.group(2).contains("*"), groups.group(2).contains("-"))
      }
      case None => {
        return (category, false, false)
      }
    }
  }
}
