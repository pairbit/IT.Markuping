using IT.Markuping.Benchmarks;
using System;

new TagFinderBenchmark().Test();
BenchmarkDotNet.Running.BenchmarkRunner.Run<TagFinderBenchmark>();

Console.WriteLine("End....");