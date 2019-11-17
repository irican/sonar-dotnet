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

using SonarAnalyzer.Helpers;

namespace SonarAnalyzer.Rules
{
    public abstract class BinaryOperationWithIdenticalExpressionsBase : SonarDiagnosticAnalyzer
    {
        internal const string DiagnosticId = "S1764";

        protected const string OperatorMessageFormat = "请修改运算符'{0}'任意一侧的值，使得它们不相同。";
        protected const string EqualsMessage = "把此实例值 '{0}' 换为不同的值，'{0}' 与自己的比较的结果总为true。";
    }
}
