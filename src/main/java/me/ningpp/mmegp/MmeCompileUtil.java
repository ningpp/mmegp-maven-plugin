/*
 *    Copyright 2021-2022 the original author or authors.
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
import java.nio.file.StandardOpenOption;
import java.util.ArrayList;
import java.util.List;
import java.util.Optional;
import java.util.stream.Collectors;

import org.mybatis.generator.api.GeneratedFile;
import org.mybatis.generator.api.GeneratedJavaFile;
import org.mybatis.generator.api.GeneratedXmlFile;
import org.mybatis.generator.api.IntrospectedTable;
import org.mybatis.generator.api.Plugin;
import org.mybatis.generator.config.Context;
import org.mybatis.generator.exception.ShellException;
import org.mybatis.generator.internal.DefaultShellCallback;

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

    private static IntrospectedTable buildIntrospectedTable(Context context, 
            String modelPackageName, String mapperPackageName, File file) throws IOException, ClassNotFoundException {
        if (!file.getName().endsWith(".java")) {
            return null;
        }
        return buildIntrospectedTable(context, modelPackageName, mapperPackageName, 
                Files.readString(file.toPath(), StandardCharsets.UTF_8));
    }

    public static IntrospectedTable buildIntrospectedTable(Context context, 
            String modelPackageName, String mapperPackageName, String fileContent) throws IOException, ClassNotFoundException {
        ParseResult<CompilationUnit> parseResult = JAVA_PARSER.parse(fileContent);
        Optional<CompilationUnit> cuOptional = parseResult.getResult();
        if (parseResult.isSuccessful() && cuOptional.isPresent()) {
            return MyBatisGeneratorUtil.buildIntrospectedTable(context, modelPackageName, mapperPackageName, cuOptional.get());
        } else {
            System.err.println(parseResult.getProblems());
            return null;
        }
    }

    public static void generate(Context context, 
            String modelPackageName, String modelFileDir, String exampleFileDir, 
            String mapperPackageName, String mapperFileDir, 
            String xmlFileDir,
            List<Plugin> plugins) throws IOException, ClassNotFoundException, ShellException {
        mkdirs(exampleFileDir);
        mkdirs(mapperFileDir);
        mkdirs(xmlFileDir);

        File[] javaFiles = new File(modelFileDir).listFiles();
        if (javaFiles == null) {
            return;
        }
        List<GeneratedXmlFile> generatedXmlFiles = new ArrayList<>();
        List<GeneratedJavaFile> additionalJavaFiles = new ArrayList<>();
        for (File file : javaFiles) {
            IntrospectedTable introspectedTable = buildIntrospectedTable(context, modelPackageName, mapperPackageName, file);
            if (introspectedTable == null) {
                 continue;
            }
            additionalJavaFiles.addAll(introspectedTable.getGeneratedJavaFiles().stream()
                        .filter(gjf -> !gjf.getFileName().equals(file.getName()))
                    .collect(Collectors.toList()));

            generatedXmlFiles.addAll(introspectedTable.getGeneratedXmlFiles());

            if (plugins != null) {
                for (Plugin plugin : plugins) {
                    plugin.initialized(introspectedTable);
                    List<GeneratedJavaFile> files = plugin.contextGenerateAdditionalJavaFiles(introspectedTable);
                    files.forEach(f -> System.out.println("generated :   " + f.getFileName()));
                    additionalJavaFiles.addAll(files);
                    generatedXmlFiles.addAll(plugin.contextGenerateAdditionalXmlFiles(introspectedTable));
                }
            }
        }

        DefaultShellCallback shellCallback = new DefaultShellCallback(true);
        for (GeneratedJavaFile gjf : additionalJavaFiles) {
            writeFile(shellCallback, gjf);
        }

        for (GeneratedXmlFile gxf : generatedXmlFiles) {
            writeFile(shellCallback, gxf);
        }
    }

    private static void writeFile(DefaultShellCallback shellCallback, GeneratedFile gf) throws ShellException, IOException {
        if (!new File(gf.getTargetProject()).exists()) {
            new File(gf.getTargetProject()).mkdirs();
        }
        File directory = shellCallback.getDirectory(gf.getTargetProject(), gf.getTargetPackage());
        File targetFile = new File(directory, gf.getFileName());
        targetFile.getParentFile().mkdirs();
        targetFile.delete();
        Files.writeString(targetFile.toPath(), gf.getFormattedContent(), 
                StandardCharsets.UTF_8, StandardOpenOption.CREATE);
        System.out.println("writed :   " + targetFile.getAbsolutePath());
    }
}
