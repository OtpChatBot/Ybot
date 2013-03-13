/*
 * Url encode/decode with Ybot
 *
 * Usage:
 *    Ybot url encode some_url
 *    Ybot url decode some_url
 */

import java.net.URLEncoder
import java.net.URLDecoder

object Url {
   def main(args: Array[String]) {
   	   // check params
       if (args.length != 1) {
           println("Wrong usage\nUsage:\nYbot url encode some_url\nYbot url decode some_url")
       }
       // get method
       val method = args(0).trim().split(" ")(0)
       // match method
       method.trim match {
           case "encode" => encode(args(0))
           case "decode" => decode(args(0))
           case _ => println("Wrong usage\nUsage:\nYbot url encode some_url\nYbot url decode some_url " + method.trim)
       }
   }

   def encode(url : String) : Unit = {
       // encode url
       println(java.net.URLEncoder.encode(url.split("encode")(1).trim(), "utf8"))
   }

   def decode(url : String) : Unit = {
       // decode url
       println(java.net.URLDecoder.decode(url.split("decode")(1).trim(), "utf8"))
   }   
}