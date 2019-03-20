import java.awt.Color;
import java.awt.Font;
import java.awt.FontMetrics;
import java.awt.Graphics2D;
import java.awt.RenderingHints;
import java.awt.image.BufferedImage;
import java.io.File;
import java.io.IOException;
import javax.imageio.ImageIO;

import java.util.List;

public class TextToImage {
  
    public static void createImage(List<String> text, String filename) {
      /*
         Because font metrics is based on a graphics context, we need to create
         a small, temporary image so we can ascertain the width and height
         of the final image
         
         Modified version of:
         https://stackoverflow.com/questions/18800717/convert-text-content-to-image
         
         Assumes passed text is all the same length (in characters)
       */
      BufferedImage img = new BufferedImage(1, 1, BufferedImage.TYPE_INT_ARGB);
      Graphics2D g2d = img.createGraphics();
      Font font = new Font(Font.MONOSPACED, Font.PLAIN, 52);
      g2d.setFont(font);
      FontMetrics fm = g2d.getFontMetrics();
      int width = fm.stringWidth(text.get(0)) + 30;
      int height = fm.getAscent() * (text.size() + 1);
      g2d.dispose();

      img = new BufferedImage(width, height, BufferedImage.TYPE_INT_RGB);
      g2d = img.createGraphics();
      g2d.setRenderingHint(RenderingHints.KEY_ALPHA_INTERPOLATION, RenderingHints.VALUE_ALPHA_INTERPOLATION_QUALITY);
      g2d.setRenderingHint(RenderingHints.KEY_ANTIALIASING, RenderingHints.VALUE_ANTIALIAS_ON);
      g2d.setRenderingHint(RenderingHints.KEY_COLOR_RENDERING, RenderingHints.VALUE_COLOR_RENDER_QUALITY);
      g2d.setRenderingHint(RenderingHints.KEY_DITHERING, RenderingHints.VALUE_DITHER_ENABLE);
      g2d.setRenderingHint(RenderingHints.KEY_FRACTIONALMETRICS, RenderingHints.VALUE_FRACTIONALMETRICS_ON);
      g2d.setRenderingHint(RenderingHints.KEY_INTERPOLATION, RenderingHints.VALUE_INTERPOLATION_BILINEAR);
      g2d.setRenderingHint(RenderingHints.KEY_RENDERING, RenderingHints.VALUE_RENDER_QUALITY);
      g2d.setRenderingHint(RenderingHints.KEY_STROKE_CONTROL, RenderingHints.VALUE_STROKE_PURE);
      g2d.setFont(font);
      fm = g2d.getFontMetrics();
      g2d.setColor(Color.WHITE);
      g2d.fillRect(0, 0, img.getWidth(), img.getHeight());
      g2d.setColor(Color.BLACK);
      int counter = 0;
      for (String s : text) {
        g2d.drawString(s, 10, fm.getAscent() * (counter + 1));
        counter++;
      }      
      g2d.dispose();
      try {
          ImageIO.write(img, "png", new File(filename));
      } catch (IOException ex) {
          ex.printStackTrace();
      }
    }

}