// Copyright (c) .NET Foundation. All rights reserved.
// Licensed under the Apache License, Version 2.0. See License.txt in the project root for license information.

using System;
using System.Collections.Generic;
using System.IO;
using System.Linq;
using System.Reflection;
using System.Runtime.CompilerServices;
using System.Text;
using System.Text.Encodings.Web;
using System.Threading.Tasks;
using BenchmarkDotNet.Attributes;
using Microsoft.AspNetCore.Html;
using Microsoft.AspNetCore.Mvc.Razor.Extensions;
using Microsoft.AspNetCore.Razor.Hosting;
using Microsoft.AspNetCore.Razor.Language;
using Microsoft.AspNetCore.Razor.Language.Extensions;
using Microsoft.CodeAnalysis;
using Microsoft.CodeAnalysis.CSharp;

namespace Microsoft.AspNetCore.Razor.Performance
{
    public class RuntimePerformanceBenchmark
    {
        public static string[] Documents { get; } = new string[]
        {
            "HelloWorld.cshtml",
            "HelperTyped.cshtml",
            "HelperDynamic.cshtml",
        };

        [ParamsSource(nameof(Documents))]
        public string Document;

        private static string GetClassName(string path) => Path.GetFileNameWithoutExtension(path);

        private MockBaseView _view;

        // runs once for every Document value
        [GlobalSetup]
        public void GlobalSetup()
        {
            var current = new DirectoryInfo(AppContext.BaseDirectory);
            while (current != null && !File.Exists(Path.Combine(current.FullName, Document)))
            {
                current = current.Parent;
            }

            var root = current;
            var fileSystem = RazorProjectFileSystem.Create(root.FullName);

            var className = GetClassName(Document);
            var viewBaseType = typeof(MockBaseView);
            var engine = RazorProjectEngine.Create(RazorConfiguration.Default, fileSystem, builder =>
            {
                builder.AddTargetExtension(new TemplateTargetExtension()
                {
                    TemplateTypeName = typeof(MockHelperResult).FullName,
                });

                builder
                    .SetNamespace(GetType().Namespace)
                    .SetBaseType(viewBaseType.FullName)
                    .ConfigureClass((document, @class) =>
                    {
                        @class.ClassName = GetClassName(document.Source.FilePath);
                        @class.Modifiers.Clear();
                        @class.Modifiers.Add("public");
                    });
            });
            var directiveFeature = engine.EngineFeatures.OfType<IRazorDirectiveFeature>().FirstOrDefault();
            var directives = directiveFeature?.Directives.ToArray() ?? Array.Empty<DirectiveDescriptor>();

            var compilation = Microsoft.CodeAnalysis.CSharp.CSharpCompilation.Create(GetType().Name + "_" + className);
            compilation = compilation.WithReferences(
                MetadataReference.CreateFromFile(Assembly.Load("netstandard").Location),
                MetadataReference.CreateFromFile(Assembly.Load("System.Runtime").Location),
                MetadataReference.CreateFromFile(Assembly.Load("System.Runtime.Extensions").Location), // required for TextWriter
                MetadataReference.CreateFromFile(Assembly.Load("Microsoft.CSharp").Location), // required for dynamic
                MetadataReference.CreateFromFile(Assembly.Load("System.Linq.Expressions").Location), // https://github.com/dotnet/roslyn/issues/23573#issuecomment-417801552
                MetadataReference.CreateFromFile(typeof(Task).Assembly.Location),
                MetadataReference.CreateFromFile(typeof(IHtmlContent).Assembly.Location),
                MetadataReference.CreateFromFile(typeof(Hosting.RazorCompiledItem).Assembly.Location),
                MetadataReference.CreateFromFile(viewBaseType.Assembly.Location)
            );
            compilation = compilation.WithOptions(
                compilation.Options
                    .WithOutputKind(OutputKind.DynamicallyLinkedLibrary)
                    .WithAssemblyIdentityComparer(
                        DesktopAssemblyIdentityComparer.Default));

            var filePath = Path.Combine(root.FullName, Document);
            var projectItem = fileSystem.GetItem(filePath);
            var codeDocument =  engine.Process(projectItem);
            var csDocument = codeDocument.GetCSharpDocument();

            foreach (var diagnostic in csDocument.Diagnostics)
            {
                Console.WriteLine($"{Document}: RAZOR {diagnostic.Id}: {diagnostic.GetMessage()}");
                if (diagnostic.Severity == RazorDiagnosticSeverity.Error)
                {
                    throw new Exception($"{Document} contains invalid cshtml");
                }
            }

            var csSyntaxTree = CSharpSyntaxTree.ParseText(
                csDocument.GeneratedCode,
                path: filePath + ".cs",
                encoding: Encoding.UTF8);
            compilation = compilation.AddSyntaxTrees(csSyntaxTree);

            using (var pe = new MemoryStream())
            using (var pdb = new MemoryStream())
            {
                var result = compilation.Emit(pe, pdb);
                foreach (var diagnostic in result.Diagnostics)
                {
                    Console.WriteLine(diagnostic.ToString());
                }
                if (!result.Success)
                {
                    throw new Exception("Compilation failed");
                }
                pe.Position = pdb.Position = 0;
                var assembly = Assembly.Load(pe.GetBuffer(), pdb.GetBuffer());
                var loader = new RazorCompiledItemLoader();
                var razorItems = loader.LoadItems(assembly);
                var item = razorItems.First(x => x.Type.Name == className);
                _view = (MockBaseView)Activator.CreateInstance(item.Type);
            }
        }


        [Benchmark(Description = "Perf-testing generated razor code")]
        public Task SyntaxTreeGeneration_DesignTime_LargeStaticFile() => _view.ExecuteAsync();
    }

    /// <summary>
    /// Must implement all the methods used in <see cref="Microsoft.AspNetCore.Razor.Language.CodeGeneration.RuntimeNodeWriter" />
    /// </summary>
    public abstract class MockBaseView
    {
        public abstract Task ExecuteAsync();

        protected void WriteLiteral(object literal)
        {
        }

        protected void BeginContext(int position, int length, bool isLiteral)
        {
        }

        protected void EndContext()
        {
        }

        protected void PushWriter(TextWriter wr)
        {
        }

        protected void PopWriter()
        {
        }

        protected void BeginWriteAttribute(string name, string prefix, int prefixOffset, string suffix, int suffixOffset, int attributeValuesCount)
        {
        }

        protected void EndWriteAttribute()
        {
        }

        protected void WriteAttributeValue(string prefix, int prefixOffset, object value, int valueOffset, int valueLength, bool isLiteral)
        {
        }

        protected void Write (object value)
        {
        }
    }

    public class MockHelperResult : IHtmlContent
    {
        private readonly Func<TextWriter, Task> _writeAction;
        public MockHelperResult(Func<TextWriter,Task> writeAction) => _writeAction = writeAction;
        // ugh https://github.com/aspnet/Mvc/blob/a2c8537dd86474a1cb5bb3ea12ada4ecabd6ee4c/src/Microsoft.AspNetCore.Mvc.Razor/HelperResult.cs#L58
        public void WriteTo(TextWriter writer, HtmlEncoder encoder) => _writeAction(writer).GetAwaiter().GetResult();
    }
}
