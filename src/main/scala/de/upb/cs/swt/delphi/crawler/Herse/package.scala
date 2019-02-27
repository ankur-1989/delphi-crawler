package de.upb.cs.swt.delphi.crawler

package object Herse {

  case class FunctionDeclaration(
                                  `type`: String,
                                  id: Id,
                                  params: List[Id],
                                  body: Body,
                                  generator: Boolean,
                                  expression: Boolean,
                                  async: Boolean
                                )
  case class Id(
                 `type`: String,
                 name: String
               )
  case class Body(body: Seq[Any])

  case class AssignmentMemberExpression(
                                         `type`: String,
                                         operator : String,
                                         left: MemberExpression,
                                         right: FunctionExpression
                                       )
  case class AssignmentIdentifierExpression(
                                             `type`: String,
                                             operator : String,
                                             left : Id,
                                             right: FunctionExpression
                                           )

  case class MemberExpression(
                               `type`: String,
                               computed: Boolean,
                               `object` : Map[String,Any],
                               property : Id
                             )


  case class VariableDeclarator (
                                  `type`: String,
                                  id: Id,
                                  init: FunctionExpression

                                )


  case class FunctionExpression(
                                 `type`: String,
                                 id : Id,
                                 params: List[Id],
                                 body : Body,
                                 generator: Boolean,
                                 expression: Boolean,
                                 async: Boolean
                               )

  case class CalleeMemberExpression(
                                     `type`: String,
                                     computed: Boolean,
                                     `object`: Option[Id],
                                     property: Id
                                   )

  case class CalleeIdentifierExpression(
                                         `type`: String,
                                         name : String
                                       )
  case class BlockComment(
                           `type` : String,
                           value: String,
                           loc: LOC
                         )
  case class LOC (
                   start: Position,
                   end: Position

                 )
  case class Position (
                        line: Int,
                        column: Int
                      )

}
