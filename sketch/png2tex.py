import imageio

im = imageio.imread('team211.png', pilmode='RGBA')
#print(im)
print(im.shape)

with open('team210.h', 'wt') as f:
    print(len(im[0]))
    f.write("// Generated with png2tex (c) 2019 Alexander Kraus <nr4@z10.info>\n");
    f.write("static const char header_data[] = {");
    for x in range(len(im[0])-1):
        for y in range(len(im[0])):
            f.write(str(im[x][y][0])+ ","+ str(im[x][y][1])+ ","+ str(im[x][y][2])+ ","+ str(im[x][y][3])+",")
    for y in range(len(im[0])):
        f.write(str(im[-1][y][0])+ ","+ str(im[-1][y][1])+ ","+ str(im[-1][y][2])+ ","+ str(im[-1][y][3])+",")
    f.write(str(im[-1][-1][0])+ ","+ str(im[-1][-1][1])+ ","+ str(im[-1][-1][2])+ ","+ str(im[-1][-1][3]))

    f.write("};\n");
    f.write("static unsigned int qrcode_texture_size = "+ str(len(im[x]))+ ";");
