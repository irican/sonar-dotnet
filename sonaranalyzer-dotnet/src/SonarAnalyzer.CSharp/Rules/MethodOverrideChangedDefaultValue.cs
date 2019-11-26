/*
 * SonarAnalyzer for .NET
 * Copyright (C) 2015-2019 SonarSource SA
 * mailto: contact AT sonarsource DOT com
 *
 * This program is free software; you can redistribute it and/or
 * modify it under the terms of the GNU Lesser General Public
 * License as published by the Free Software Foundation; either
 * version 3 of the License, or (at your option) any later version.
 *
 * This program is distributed in the hope that it will be useful,
 * but WITHOUT ANY WARRANTY; without even the implied warranty of
 * MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the GNU
 * Lesser General Public License for more details.
 *
 * You should have received a copy of the GNU Lesser General Public License
 * along with this program; if not, write to the Free Software Foundation,
 * Inc., 51 Franklin Street, Fifth Floor, Boston, MA  02110-1301, USA.
 */

using System.Collections.Immutable;
using System.Linq;
using Microsoft.CodeAnalysis;
using Microsoft.CodeAnalysis.CSharp;
using Microsoft.CodeAnalysis.CSharp.Syntax;
using Microsoft.CodeAnalysis.Diagnostics;
using SonarAnalyzer.Common;
using SonarAnalyzer.Helpers;

namespace SonarAnalyzer.Rules.CSharp
{
    [DiagnosticAnalyzer(LanguageNames.CSharp)]
    [Rule(DiagnosticId)]
    public sealed class MethodOverrideChangedDefaultValue : SonarDiagnosticAnalyzer
    {
        internal const string DiagnosticId = "S1006";
        private const string MessageFormat = "{0}默认参数值，{1}。";
        internal const string MessageAdd = "在重载方法中定义的";
        internal const string MessageRemove = "以符合重载方法的签名";
        internal const string MessageUseSame = "在重载方法中定义的";
        internal const string MessageRemoveExplicit = "从明确的接口实现中";

        private static readonly DiagnosticDescriptor rule =
            DiagnosticDescriptorBuilder.GetDescriptor(DiagnosticId, MessageFormat, RspecStrings.ResourceManager);

        public override ImmutableArray<DiagnosticDescriptor> SupportedDiagnostics { get; } = ImmutableArray.Create(rule);

        protected override void Initialize(SonarAnalysisContext context)
        {
            context.RegisterSyntaxNodeActionInNonGenerated(
                c =>
                {
                    var method = (MethodDeclarationSyntax)c.Node;
                    var methodSymbol = c.SemanticModel.GetDeclaredSymbol(method);

                    var overriddenMember = methodSymbol.GetOverriddenMember() ?? methodSymbol.GetInterfaceMember();
                    if (methodSymbol == null ||
                        overriddenMember == null)
                    {
                        return;
                    }

                    for (var i = 0; i < methodSymbol.Parameters.Length; i++)
                    {
                        var overridingParameter = methodSymbol.Parameters[i];
                        var overriddenParameter = overriddenMember.Parameters[i];

                        var parameterSyntax = method.ParameterList.Parameters[i];

                        ReportParameterIfNeeded(overridingParameter, overriddenParameter, parameterSyntax,
                            isExplicitImplementation: methodSymbol.ExplicitInterfaceImplementations.Any(),
                            context: c);
                    }
                },
                SyntaxKind.MethodDeclaration);
        }

        private static void ReportParameterIfNeeded(IParameterSymbol overridingParameter, IParameterSymbol overriddenParameter,
            ParameterSyntax parameterSyntax, bool isExplicitImplementation, SyntaxNodeAnalysisContext context)
        {
            if (isExplicitImplementation)
            {
                if (overridingParameter.HasExplicitDefaultValue)
                {
                    context.ReportDiagnosticWhenActive(Diagnostic.Create(rule, parameterSyntax.Default.GetLocation(), "去除", MessageRemoveExplicit));  //remove
                }

                return;
            }

            if (overridingParameter.HasExplicitDefaultValue &&
                !overriddenParameter.HasExplicitDefaultValue)
            {
                context.ReportDiagnosticWhenActive(Diagnostic.Create(rule, parameterSyntax.Default.GetLocation(), "去除", MessageRemove));  //remove
                return;
            }

            if (!overridingParameter.HasExplicitDefaultValue &&
                overriddenParameter.HasExplicitDefaultValue)
            {
                context.ReportDiagnosticWhenActive(Diagnostic.Create(rule, parameterSyntax.Identifier.GetLocation(), "添加", MessageAdd));  //add
                return;
            }

            if (overridingParameter.HasExplicitDefaultValue &&
                overriddenParameter.HasExplicitDefaultValue &&
                !Equals(overridingParameter.ExplicitDefaultValue, overriddenParameter.ExplicitDefaultValue))
            {
                context.ReportDiagnosticWhenActive(Diagnostic.Create(rule, parameterSyntax.Default.Value.GetLocation(), "使用", MessageUseSame));  //use
            }
        }
    }
}
