/*
 *    Copyright 2021 the original author or authors.
 *
 *    Licensed under the Apache License, Version 2.0 (the "License");
 *    you may not use this file except in compliance with the License.
 *    You may obtain a copy of the License at
 *
 *       http://www.apache.org/licenses/LICENSE-2.0
 *
 *    Unless required by applicable law or agreed to in writing, software
 *    distributed under the License is distributed on an "AS IS" BASIS,
 *    WITHOUT WARRANTIES OR CONDITIONS OF ANY KIND, either express or implied.
 *    See the License for the specific language governing permissions and
 *    limitations under the License.
 */
package me.ningpp.mmegp;

import java.io.File;
import java.io.IOException;
import java.nio.charset.StandardCharsets;
import java.nio.file.Files;
import java.nio.file.Paths;
import java.nio.file.StandardOpenOption;
import java.util.ArrayList;
import java.util.List;
import java.util.Map.Entry;
import java.util.Optional;

import org.mybatis.generator.api.GeneratedJavaFile;
import org.mybatis.generator.api.GeneratedXmlFile;
import org.mybatis.generator.api.IntrospectedTable;
import org.mybatis.generator.api.Plugin;
import org.mybatis.generator.api.dom.DefaultJavaFormatter;
import org.mybatis.generator.api.dom.java.Interface;
import org.mybatis.generator.api.dom.java.TopLevelClass;
import org.mybatis.generator.config.Context;

import com.github.javaparser.JavaParser;
import com.github.javaparser.ParseResult;
import com.github.javaparser.ParserConfiguration;
import com.github.javaparser.ast.CompilationUnit;

public final class MmeCompileUtil {
    private MmeCompileUtil() {
    }

    private static final JavaParser JAVA_PARSER;

    static {
        ParserConfiguration jpc = new ParserConfiguration();
        jpc.setCharacterEncoding(StandardCharsets.UTF_8);
        JAVA_PARSER = new JavaParser();
    }

    private static void mkdirs(String dir) throws IOException {
        if (! new File(dir).exists()) {
            boolean mkdirs = new File(dir).mkdirs();
            if (!mkdirs) {
                throw new IOException("mkdirs fail! dir = " + dir);
            }
        }
    }

    private static Interface buildMapperInterface(Context context, IntrospectedTable introspectedTable) {
        MmeJavaMapperGenerator javaMapperGenerator = new MmeJavaMapperGenerator("", false);
        javaMapperGenerator.setContext(context);
        javaMapperGenerator.setIntrospectedTable(introspectedTable);
        return (Interface) javaMapperGenerator.getCompilationUnits().get(0);
    }

    private static Entry<IntrospectedTable, TopLevelClass> buildExampleClass(Context context, 
            String modelPackageName, String mapperPackageName, File file) throws IOException, ClassNotFoundException {
        if (!file.getName().endsWith(".java") || file.getName().endsWith("Example.java")) {
            return null;
        }
        ParseResult<CompilationUnit> parseResult = JAVA_PARSER.parse(file);
        Optional<CompilationUnit> cuOptional = parseResult.getResult();
        if (parseResult.isSuccessful() && cuOptional.isPresent()) {
            return MyBatisGeneratorUtil.buildCompilationUnit(context, modelPackageName, mapperPackageName, cuOptional.get());
        } else {
            System.err.println(parseResult.getProblems());
            return null;
        }
    }

    public static void generate(Context context, 
            String modelPackageName, String modelFileDir, String exampleFileDir, 
            String mapperPackageName, String mapperFileDir, 
            String xmlFileDir,
            Plugin mapperExtGeneratorPlugin,
            Plugin mapperExtXmlGeneratorPlugin) throws IOException, ClassNotFoundException {
        mkdirs(exampleFileDir);
        mkdirs(mapperFileDir);
        mkdirs(xmlFileDir);

        File[] javaFiles = new File(modelFileDir).listFiles();
        List<org.mybatis.generator.api.dom.java.CompilationUnit> tlcList = new ArrayList<>();
        List<GeneratedXmlFile> generatedXmlFiles = new ArrayList<>();
        List<GeneratedJavaFile> generatedMapperExtJavaFiles = new ArrayList<>();
        for (File file : javaFiles) {
            Entry<IntrospectedTable, TopLevelClass> exampleClassEntry = buildExampleClass(context, modelPackageName, mapperPackageName, file);
            if (exampleClassEntry != null) {
                tlcList.add(exampleClassEntry.getValue());

                IntrospectedTable introspectedTable = exampleClassEntry.getKey();
                Interface mapperClass = buildMapperInterface(context, introspectedTable);
                if (mapperClass != null) {
                    tlcList.add(mapperClass);
                }

                if (mapperExtGeneratorPlugin != null) {
                    generatedMapperExtJavaFiles.addAll(mapperExtGeneratorPlugin.contextGenerateAdditionalJavaFiles(introspectedTable));
                }
                if (mapperExtXmlGeneratorPlugin != null) {
                    generatedXmlFiles.addAll(mapperExtXmlGeneratorPlugin.contextGenerateAdditionalXmlFiles(introspectedTable));
                }

                generatedXmlFiles.addAll(introspectedTable.getGeneratedXmlFiles());
            }
        }

        for (GeneratedXmlFile generatedXmlFile : generatedXmlFiles) {
            String xmlFile = xmlFileDir + File.separator + generatedXmlFile.getFileName();
            new File(xmlFile).delete();
            Files.writeString(Paths.get(xmlFile), generatedXmlFile.getFormattedContent(), 
                    StandardCharsets.UTF_8, StandardOpenOption.CREATE);
        }

        for (GeneratedJavaFile generatedMapperExtJavaFile : generatedMapperExtJavaFiles) {
            String mapperExtJavaFile = mapperFileDir + File.separator + generatedMapperExtJavaFile.getFileName();
            new File(mapperExtJavaFile).delete();
            Files.writeString(Paths.get(mapperExtJavaFile), generatedMapperExtJavaFile.getFormattedContent(), 
                    StandardCharsets.UTF_8, StandardOpenOption.CREATE);
        }
        
        DefaultJavaFormatter javaFormatter = new DefaultJavaFormatter();
        for (org.mybatis.generator.api.dom.java.CompilationUnit compilationUnit : tlcList) {
            String dir;
            if (compilationUnit.getType().getShortName().endsWith("Example")) {
                dir = exampleFileDir;
            } else {
                dir = mapperFileDir;
            }
            String javaFile = dir + compilationUnit.getType().getShortName() + ".java";
            new File(javaFile).delete();
            Files.writeString(Paths.get(javaFile), javaFormatter.getFormattedContent(compilationUnit), 
                    StandardCharsets.UTF_8, StandardOpenOption.CREATE);
        }
    }

}
