import java.io.File
import java.net.InetSocketAddress
import java.nio.channels.{ServerSocketChannel, SocketChannel}
import java.nio.file.{StandardOpenOption, Files, Paths}
import java.nio.{ByteBuffer, ByteOrder}

import org.opencv.core.{Size, Mat, Core}
import org.opencv.imgcodecs.Imgcodecs
import org.opencv.imgproc.Imgproc

/**
  * Created by yang on 15-11-30.
  */
class ModelServer(port: Int,imageFoldPath: String) {
  val server = ServerSocketChannel.open().bind(new InetSocketAddress(9000))

  val faces = Array("front","side1","back","side2")

  var imagedir = ""

  var imageFaceSizes: Array[Int] = null

  /**
    * try to run the server
    */
  def runserver(): Unit = {
    var flag = true
    while(flag){
      // wait for the socket to link in

      val socket = server.accept()
      println(s"have a link from ${socket.getLocalAddress.toString}")

      // try to handle the socket
      handle(socket)
    }
  }

  /**
    * create the dir for store the image and four sub folder to store front
    * side1 back side2s
    * @return the image dir to store
    */
  private def createFold() = {
    val path = Paths.get(imageFoldPath)
    if(!Files.exists(path)){
      Files.createDirectories(path)
    }

    val time = System.currentTimeMillis().toString
    // create now fold to save the image
    val imagePath = path.resolve(time)
    if(!Files.exists(imagePath))
      Files.createDirectories(imagePath)

    // create four sub folder to store different side image
    faces.foreach(face => Files.createDirectories(imagePath.resolve(face)))

    imagedir = imagePath.toString
  }

  private def saveImage(imagedir: String,imageIndex: Int, imageBuffer: ByteBuffer) = {
    // try to get the imageIndex to 3 chars string
    val imagePath = Paths.get(imagedir, f"$imageIndex%05d.jpg")
    Files.write(imagePath,imageBuffer.array())

    // try to resize the image
    val imageMat = Imgcodecs.imread(imagePath.toString)
    val (width,height) = (imageMat.width()/2,imageMat.height()/2)
    val dstMat = new Mat(width,height,imageMat.`type`())
    Imgproc.resize(imageMat,dstMat,new Size(width,height))

    Imgcodecs.imwrite(imagePath.toString,dstMat)

    println(s"save image $imageIndex well")
  }

  /**
    * use the image to build the 3d model
    */
  private def modelBuild(): Unit = {
    import collection.JavaConversions._
    import collection.JavaConverters._
    import sys.process._

    def runTime(f: =>Unit): Double = {
      val start = System.currentTimeMillis()
      f
      val end = System.currentTimeMillis()
      (end - start)*1.0/1000
    }

    // first to run to get the act
    def runAct(dir: String) = {
      // config the commond to run libsfm
      val first = 0.toString
      val step = 1.toString
      val end = (Files.list(Paths.get(dir)).count() - 1).toString

      Process(Seq("TestLibSfM/x64/Release/TestLibSfM.exe", "../../../" + dir + "/00000.jpg",
        "seq.act", first, step, end), new File("TestLibSfM/x64/Release/")).!
    }

    // second to run to get the depth
    def runDepth(dir: String) = {
      // change config file
      val path = Paths.get("VDR_X64","config.txt")
      val lines: Array[String] = Files.lines(path).iterator().toArray
      val newProjectDir = "../" + dir + "/seq.act"
      lines(0) = s"project_file $newProjectDir"
      Files.write(path,lines.toIterable.asJava)

      Process("VDR_X64/VideoDepthRecovery.exe", new File("VDR_X64")).!
    }

    // three to run to get the model
    def runModel = {
      // change config file
      val appdir = "MultiViewStitch"
      val path = Paths.get("MultiViewStitch/imgPathList.txt")
      val writer = Files.newBufferedWriter(path,StandardOpenOption.WRITE);
      faces.foreach(face => {
        writer.write(s"../$imagedir/$face/")
        writer.newLine()
      })
      writer.flush()
      writer.close()

      // run the model rebuild
      Process(Seq("MultiViewStitch/MultiViewStitch.exe","config.txt"),new File(appdir)).!
    }
    println("receive image end and begin to build the model")

    var actTime = 0.0;
    var depthTime = 0.0;

    faces.foreach(face => {
      val dir = imagedir + "/" + face

      actTime += runTime(runAct(dir))
      depthTime += runTime(runDepth(dir))
    })

    val modelTime = runTime(runModel)
    println("get model end")
    println(s"time usage: act time $actTime depth time $depthTime modelTime $modelTime")
  }

  /**
    * receive four int from the client
    * @param input the client socket channel
    * @return
    */
  def receiveSizes(input: SocketChannel): Array[Int] = {
    val buffer = receiveSizeData(input,4 * 4)
    Range(0,4).foldLeft(Array[Int]()) { (arrs, _) =>
      arrs :+ buffer.getInt()
    }
  }

  /**
    * received size data from the client,and return the data
    * @param input the client socket channel
    * @param size the size want to read
    * @return the data get from the client
    */
  def receiveSizeData(input: SocketChannel,size: Int): ByteBuffer = {
    var readSize = 0
    val byteBuffer = ByteBuffer.allocate(size).order(ByteOrder.nativeOrder())

    while(readSize != -1 && byteBuffer.hasRemaining){
      readSize = input.read(byteBuffer)
    }

    byteBuffer.flip()
    byteBuffer
  }

  /**
    * handle the socket input and such as save the image , run the act
    * then the depth ,then 3d model
    * @param socket the client socket
    */
  private def handle(socket: SocketChannel): Unit = {
    // first to create the folder to store the image
    createFold()

    // try to read the image from the socket and save it
    val input = socket

    var flag = true
    var imageIndex = 0

    // try to get the different face size
    imageFaceSizes = receiveSizes(input)


    while(flag){
      // try to get the data info
      val dataInfo = receiveDataInfo(input)
      flag = dataInfo._1
      val size = dataInfo._2

      if(flag){
        receiveData(input,size,imageIndex)
        imageIndex += 1
      }
    }

    // have save all image and now can do something else
    modelBuild()
  }

  /**
    * receive the data info header from the client,such as the data size and type
    * @param input the client input channel
    * @return
    */
  private def receiveDataInfo(input: SocketChannel): (Boolean,Int) = {
    val byteBuffer = ByteBuffer.allocate(12).order(ByteOrder.nativeOrder())

    var readSize = 0
    while (byteBuffer.hasRemaining && readSize != -1) {
      readSize = input.read(byteBuffer)
    }

    byteBuffer.flip()
    val size = byteBuffer.getInt()

    if (size == -1) return (false, -1)

    val strBytes = Range(0, 5).foldLeft(Array[Byte]())((bytes, _) => {
      bytes :+ byteBuffer.get()
    })

    val typeStr = new String(strBytes)

    println(s"DATA INFO:Size:${size / 1024.0} KB, Format: $typeStr\n")

    (true,size)
  }

  /**
    * receive data from the client,and save the data to a image
    * @param input the socketchannel
    * @param size the size of the image
    * @param imageIndex the index of the image
    */
  private def receiveData(input: SocketChannel,size: Int,imageIndex: Int): Unit ={
    // try to receive the size data and save it as a image file
    val imageBuffer = ByteBuffer.allocate(size).order(ByteOrder.nativeOrder())
    var revSize = 0
    while(size > revSize){
      val readSize = input.read(imageBuffer)
      revSize += readSize
      //println(s"now receive size $readSize total $revSize remain ${size - revSize}")
    }

    // create the image file
    // check the directory and index to save the image
    val (subdir,index) = checkDir(imageIndex)
    saveImage(imagedir+"/" + subdir,index,imageBuffer)
  }

  private def checkDir(imageIndex: Int): (String,Int) = {
    val sizes = (0 +: imageFaceSizes).scanLeft(0)((sum,num) => sum + num).drop(1)
    val i = sizes.count(_ < imageIndex + 1)
    (faces(i-1),imageIndex - sizes(i-1))
  }

}

object ModelServer{
  def main(args: Array[String]) {

    System.loadLibrary(Core.NATIVE_LIBRARY_NAME)

    println(Paths.get(".").toAbsolutePath.toString)
    val server = new ModelServer(9000,"./images")

    server.runserver()
  }
}