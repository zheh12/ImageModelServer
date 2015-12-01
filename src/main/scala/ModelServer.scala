import java.net.InetSocketAddress
import java.nio.channels.{ServerSocketChannel, SocketChannel}
import java.nio.file.{Files, Paths}
import java.nio.{ByteBuffer, ByteOrder}

/**
  * Created by yang on 15-11-30.
  */
class ModelServer(port: Int,imageFoldPath: String) {
  val server = ServerSocketChannel.open().bind(new InetSocketAddress(9000))

  var imagedir = "";
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
    * create the dir for store the image
    * @return the image dir to store
    */
  private def createFold() = {
    val path = Paths.get(imageFoldPath);
    if(!Files.exists(path)){
      Files.createDirectories(path)
    }

    val time = System.currentTimeMillis().toString
    // create now fold to save the image
    val imagePath = path.resolve(time)
    if(!Files.exists(imagePath))
      Files.createDirectories(imagePath)
    imagedir = imagePath.toString
  }

  private def saveImage(imagedir: String,imageIndex: Int, imageBuffer: ByteBuffer) = {
    // try to get the imageIndex to 3 chars string
    val imagePath = Paths.get(imagedir,f"${imageIndex}%03d.jpg")
    Files.write(imagePath,imageBuffer.array())

    println(s"save image $imageIndex well")
  }

  /**
    * use the image to build the 3d model
    */
  private def modelBuild(): Unit = {
    // first to run to get the act
    def runAct = {
      // config the commond to run libsfm
    }

    // second to run to get the depth
    def runDepth = {
      // config the config.txt to run depth vdr
    }

    // three to run to get the model
    def runModel = {
      // config
    }
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
    while(flag){
      // try to get the data info
      val dataInfo = receiveDataInfo(input)
      flag = dataInfo._1
      val size = dataInfo._2

      if(flag){
        // try to receive the size data and save it as a image file
        val imageBuffer = ByteBuffer.allocate(size).order(ByteOrder.nativeOrder())
        var revSize = 0;
        while(size > revSize){
          val readSize = input.read(imageBuffer)
          println(s"now receive size $readSize total receive $revSize")
          revSize += readSize
        }

        // create the image file
        imageIndex += 1
        saveImage(imagedir,imageIndex,imageBuffer)
      }
    }

    // have save all image and now can do something else
    modelBuild()
  }

  /**
    * receive the data info header from the client,such as the data size and type
    * @param input
    * @return
    */
  private def receiveDataInfo(input: SocketChannel): (Boolean,Int) = {
    val byteBuffer = ByteBuffer.allocate(12).order(ByteOrder.nativeOrder())

    while (byteBuffer.hasRemaining) {
      input.read(byteBuffer)
    }

    byteBuffer.flip()
    val size = byteBuffer.getInt()

    if(size == -1) return (false,-1);

    val strBytes = Range(0, 5).foldLeft(Array[Byte]())((bytes, _) => {
      println(bytes.size)
      bytes :+ byteBuffer.get()
    })

    val typeStr = new String(strBytes)

    println(s"DATA INFO:Size:${size / 1024.0} KB, Format: ${typeStr}\n")

    (true,size)
  }

}

object ModelServer{
  def main(args: Array[String]) {
    val server = new ModelServer(9000,"./images")

    server.runserver()
  }
}