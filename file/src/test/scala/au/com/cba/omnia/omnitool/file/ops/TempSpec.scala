//   Copyright 2014 Commonwealth Bank of Australia
//
//   Licensed under the Apache License, Version 2.0 (the "License");
//   you may not use this file except in compliance with the License.
//   You may obtain a copy of the License at
//
//     http://www.apache.org/licenses/LICENSE-2.0
//
//   Unless required by applicable law or agreed to in writing, software
//   distributed under the License is distributed on an "AS IS" BASIS,
//   WITHOUT WARRANTIES OR CONDITIONS OF ANY KIND, either express or implied.
//   See the License for the specific language governing permissions and
//   limitations under the License.

package au.com.cba.omnia.omnitool.file.ops

import java.io.{File, FileInputStream}

import java.util.zip.GZIPInputStream

import com.google.common.base.Charsets
import com.google.common.io.Files

import au.com.cba.omnia.test.Spec

object TempSpec extends Spec { def is = s2"""
DateFormatSpec
==============

withTempFile should:
  create directory but not file before running action    $unoccupied
  delete directory and file after action                 $cleanup
  fail if other files created in temporary directory     $noTrespassers

withTempCompressed should:
  provide gzipped version of file                        $zipped

withTempModified should:
  provide identical file if modification function is id  $identity
"""

  def unoccupied = {
    Temp.withTempFile("temp", f => {
      f.exists must beFalse
      f.getParentFile.exists must beTrue
    })
    ok
  }

  def cleanup = {
    var tempFile = Option.empty[File]
    Temp.withTempFile("temp", f => {
      tempFile = Some(f)
      f.createNewFile must beTrue
    })
    tempFile must beLike {
      case Some(f) => {
        f.exists must beFalse
        f.getParentFile.exists must beFalse
      }
    }
  }

  def noTrespassers =
    // trespassers will fail to be prosecuted
    Temp.withTempFile("temp", f => {
      (new File(f.getParentFile, "trespasser")).createNewFile must beTrue
    }) must throwA[Exception]

  def zipped = {
    Temp.withTempFile("raw", raw => {
      Files.write("contents", raw, Charsets.UTF_8)
      Temp.withTempCompressed(raw, "zipped", zipped => {
        val reader = new GZIPInputStream(new FileInputStream(zipped))
        try {
          val bytes = Array.ofDim[Byte](10)
          val numRead = reader.read(bytes)
          (new String(bytes, 0, numRead)) must_== "contents"
        }
        finally {
          reader.close
        }
      })
    })
    ok
  }

  def identity = {
    Temp.withTempFile("raw", raw => {
      Files.write("contents", raw, Charsets.UTF_8)
      Temp.withTempModified(raw, "copy", in => in, copy => {
        val reader = new FileInputStream(copy)
        try {
          val bytes = Array.ofDim[Byte](10)
          val numRead = reader.read(bytes)
          (new String(bytes, 0, numRead)) must_== "contents"
        }
        finally {
          reader.close
        }
      })
    })
  }
}
