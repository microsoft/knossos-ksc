
// The parameters of a VGG network.
// Normally this will just be filled with pretrained weights 
// loaded from a file, but see vgg16_init for a typical architecture.
public class vgg_weights {
    public Tensor[][] Wconv;
    public Mat[] Wfull;
};

// Apply the VGG16 network with parameters "weights" to an image "x"
Vec vgg16(vgg_weights weights, Tensor x);

// Network "architecture" is defined by the function 'vgg16' 
// and the shapes of the weight matrices/tensors with which
// it was trained.
vgg_weights vgg16_init();


class MyTestApplication
{
    // Load a list of images, and print out their most probable class label
    static void Main(string[] args)
    {
        // Load the network parameters from some file I was given
        vgg_weights ws = deserialize<vgg_weights>("vgg_weights.onnx");

        // Classify each image 
        foreach(var filename in args) {
            Image img = imread(filename);
            Vec class_probabilities = vgg16(ws, img2tensor(img));
            Console.WriteLine("Class = {0}", argmax(class_probabilities));
        }
    }
}

class MyRetrainApplication
{
    // Fine-tune a VGG16 on our 10-class examples.  
    // Inputs: A set of image filenames, with assumed ground-truth labels in adjacent files.
    // E.g.
    //  image001.png, image001.label
    //  image002.png, image002.label
    //  etc
    static void Train(string[] images)
    {
        vgg_weights ws = deserialize<vgg_weights>("vgg_weights.onnx");
        
        // Replace last fully-connected layer with a 10xN matrix:
        int last_layer_index = Wfull.Length-1;
        Wfull[last_layer_index] = rand(10, Wfull[last_layer_index].NumCols);

        // And retrain on our examples [* Note 2 *]
        // 1. Define loss.
        loss = ws => {
            double sum = 0;
            foreach(var image in images) {  // [* Note 3 *] 
                Image img = imread(image);
                int label = int.Parse(readlines(replace_suffix(image, ".label"))[0]);
                Vec prediction = vgg16(ws, img2vec(img));
                sum += crossentropy(prediction, label);
            }
            return sum;
        };
        
        // 2. Optimize using ADAM
        ws = adam(loss, ws);

        // And check quality
        int num_correct = images.Map(image => argmax(vgg16(ws, img2tensor(imread(img)))) == 
                                              int.Parse(readlines(replace_suffix(image, ".label"))[0])).Sum();
        Console.WriteLine("Training accuracy {0}/{1}", num_correct / images.Length);
    }
}

// [* Note 2 *]
// Initially, maybe we can't provide these ergonomics, and the user needs to move this training code to F#.

// [* Note 3 *]
// This Zip is recognized and data-parallelized.   
// An equivalent foreach or foreach ( var it in someCollection.Select((x,i) => new { Value = x, Index=i }) )
// should also be recognized and data-parallelized.   Yes, this is all hard work, but we can do it.

