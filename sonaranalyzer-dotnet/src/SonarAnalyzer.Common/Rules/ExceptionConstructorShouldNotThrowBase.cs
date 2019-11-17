﻿/*
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

using Microsoft.CodeAnalysis;
using Microsoft.CodeAnalysis.Diagnostics;
using SonarAnalyzer.Helpers;
using System.Collections.Generic;
using System.Linq;

namespace SonarAnalyzer.Rules
{
    public abstract class ExceptionConstructorShouldNotThrowBase : SonarDiagnosticAnalyzer
    {
        protected const string DiagnosticId = "S3693";
        protected const string MessageFormat = "请避免在这个构造器中抛出异常。";

        protected abstract DiagnosticDescriptor Rule {get;}
        protected void ReportAllThrowLocations(SyntaxNodeAnalysisContext c,
            List<List<SyntaxNode>> throwStatementsPerCtor)
        {
            foreach (var throwStatement in throwStatementsPerCtor)
            {
                c.ReportDiagnosticWhenActive(Diagnostic.Create(Rule, throwStatement.First().GetLocation(),
                    throwStatement.Skip(1).Select(@throw => @throw.GetLocation())));
            }
        }

    }
}
