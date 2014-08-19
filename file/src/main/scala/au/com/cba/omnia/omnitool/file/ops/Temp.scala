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

import java.io.{File, FileOutputStream}

import java.util.zip.GZIPOutputStream

import com.google.common.io.Files

/**
  * Low-level operations with temporary files or directories.
  *
  * Low-level operations on temporary files that we find useful elsewhere.
  * There is no attempt to turn this into a desirable File API.
  */
object Temp {

  /**
    * Operate on a temporary and unique file.
    *
    * The file path will be unique, and is automatically deleted after`action`
    * finishes. However, the file is not automatically created on disk, as it is
    * assumed that `action` will create the file itself. For convienience,
    * the file's parent directory is automatically created on disk for the
    * duration of `action`.
    */
  def withTempFile[A](action: File => A): A =
    withTempFile("tempFile", action)

  /**
    * Operate on a temporary and unique file with a given file name.
    *
    * The file path will be unique, and is automatically deleted after`action`
    * finishes. However, the file is not automatically created on disk, as it is
    * assumed that `action` will create the file itself. For convienience,
    * the file's parent directory is automatically created on disk for the
    * duration of `action`.
    */
  def withTempFile[A](fileName: String, action: File => A): A = {
    // there must be a more convienient canonical way of doing this in scala ...
    val tempDir = Files.createTempDir
    val tempFile = new File(tempDir, fileName)
    try {
      action(tempFile)
    }
    finally {
      // Assuming the tempFile reference did not escape action
      // and nobody else is doing anything with the temorary directory.
      // Tries to delete all temporary files (the file and it's directory),
      // but bails if there are any other temporary files. If you want a place
      // to use lot's of temporary files, we should use a withTempDir method.
      if (tempFile.exists && !tempFile.delete) {
        throw new Exception(s"can't delete temporary file $tempFile")
      }
      if (tempDir.exists  && !tempDir.delete) {
        throw new Exception(s"can't delete temporary directory $tempDir")
      }
    }
  }

  /**
    * Run an action on a temporary gzipped copy of a file.
    *
    * The temporary file reference should not escape `action`.
    */
  def withTempCompressed[A](source: File, fileName: String, action: File => A): A =
    withTempFile(fileName, tempFile => {
      // there must be a more convienient canonical way of doing this in scala ...
      val writer = new GZIPOutputStream(new FileOutputStream(tempFile))
      try {
        Files.copy(source, writer)
        writer.flush
      }
      finally {
        writer.close
      }
      action(tempFile)
    })
}
