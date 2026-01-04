using IT.Markuping.Benchmarks;
using System;

new MarkupFinderBenchmark().Test();
BenchmarkDotNet.Running.BenchmarkRunner.Run<MarkupFinderBenchmark>();

Console.WriteLine("End....");