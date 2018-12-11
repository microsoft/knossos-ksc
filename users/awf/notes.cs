//1. Create ML.NET context/environment
env = LocalEnvironment()

//2. Create DataReader with data schema mapped to file's columns
reader = TextLoader(env, Separator = "tab",
                         HasHeader = true,
                         Column = {
                            { "Label", Bool, 0 },
                            { "Text", Text, 1 }
                        });

//Load training data
trainingDataView = reader.Read(MultiFileSource(TrainDataPath));

//3.Create a flexible pipeline (composed by a chain of estimators) for creating/traing the model.
pipeline = TextTransform(env, "Text", "Features")  //Convert the text column to numeric vectors (Features column)   
pipeline.Append(LinearClassificationTrainer(env, "Features", "Label")); //(Simpler in ML.NET v0.7)
