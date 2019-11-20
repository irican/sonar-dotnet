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
using Microsoft.CodeAnalysis;
using Microsoft.CodeAnalysis.CSharp.Syntax;
using Microsoft.CodeAnalysis.Diagnostics;
using SonarAnalyzer.Common;
using SonarAnalyzer.Helpers;

namespace SonarAnalyzer.Rules.CSharp
{
    [DiagnosticAnalyzer(LanguageNames.CSharp)]
    [Rule(DiagnosticId)]
    public sealed class StaticFieldWrittenFromInstanceMember : StaticFieldWrittenFrom
    {
        internal const string DiagnosticId = "S2696";
        private const string MessageFormat = "{0}";
        internal const string MessageFormatMultipleOptions = "使其外围实例 {0} 为'static'的，或移除对此'static'字段的设置。";
        internal const string MessageFormatRemoveSet = "移除这个设置，它在实例{0}中更新了一个'static'字段。";

        private static readonly DiagnosticDescriptor rule =
            DiagnosticDescriptorBuilder.GetDescriptor(DiagnosticId, MessageFormat, RspecStrings.ResourceManager);
        public override ImmutableArray<DiagnosticDescriptor> SupportedDiagnostics { get; } = ImmutableArray.Create(rule);

        protected override bool IsValidCodeBlockContext(SyntaxNode node, ISymbol owningSymbol)
        {
            if (owningSymbol == null || owningSymbol.IsStatic)
            {
                return false;
            }

            SyntaxNode declaration = node as MethodDeclarationSyntax;
            if (declaration == null)
            {
                declaration = node as AccessorDeclarationSyntax;
                if (declaration == null)
                {
                    return false;
                }
            }

            return true;
        }

        protected override string GetDiagnosticMessageArgument(SyntaxNode node, ISymbol owningSymbol, IFieldSymbol field)
        {
            var messageFormat = owningSymbol.IsChangeable()
                               ? MessageFormatMultipleOptions
                               : MessageFormatRemoveSet;
            var declarationType = GetDeclarationType(node);

            return string.Format(messageFormat, declarationType);
        }

        private static string GetDeclarationType(SyntaxNode declaration)
        {
            if (declaration is MethodDeclarationSyntax)
            {
                return "method";
            }

            if (declaration is AccessorDeclarationSyntax)
            {
                return "property";
            }

            return string.Empty;
        }
    }
}
