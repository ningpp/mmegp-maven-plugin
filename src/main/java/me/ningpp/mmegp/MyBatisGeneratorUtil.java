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

import java.math.BigDecimal;
import java.math.BigInteger;
import java.time.LocalDate;
import java.time.LocalDateTime;
import java.time.LocalTime;
import java.util.Collections;
import java.util.Date;
import java.util.HashMap;
import java.util.HashSet;
import java.util.List;
import java.util.Map;
import java.util.Optional;
import java.util.Set;

import org.apache.commons.lang3.tuple.Pair;
import org.apache.ibatis.type.JdbcType;
import org.mybatis.generator.api.FullyQualifiedTable;
import org.mybatis.generator.api.IntrospectedColumn;
import org.mybatis.generator.api.IntrospectedTable;
import org.mybatis.generator.config.Context;
import org.mybatis.generator.config.GeneratedKey;
import org.mybatis.generator.config.TableConfiguration;
import org.mybatis.generator.internal.NullProgressCallback;
import org.mybatis.generator.internal.ObjectFactory;

import com.github.javaparser.ast.CompilationUnit;
import com.github.javaparser.ast.body.ClassOrInterfaceDeclaration;
import com.github.javaparser.ast.body.FieldDeclaration;
import com.github.javaparser.ast.body.TypeDeclaration;

import me.ningpp.mmegp.annotations.Generated;

public final class MyBatisGeneratorUtil {

    private static final Map<Class<?>, JdbcType> JDBC_TYPE_MAPPING = new HashMap<>();

    private static final Set<String> EXCLUDE_IMPORTS = new HashSet<>();

    static {
        JDBC_TYPE_MAPPING.put(Boolean.class, JdbcType.BOOLEAN);
        JDBC_TYPE_MAPPING.put(boolean.class, JdbcType.BOOLEAN);

        JDBC_TYPE_MAPPING.put(Byte.class, JdbcType.TINYINT);
        JDBC_TYPE_MAPPING.put(byte.class, JdbcType.TINYINT);

        JDBC_TYPE_MAPPING.put(Short.class, JdbcType.SMALLINT);
        JDBC_TYPE_MAPPING.put(short.class, JdbcType.SMALLINT);

        JDBC_TYPE_MAPPING.put(Integer.class, JdbcType.INTEGER);
        JDBC_TYPE_MAPPING.put(int.class, JdbcType.INTEGER);

        JDBC_TYPE_MAPPING.put(Long.class, JdbcType.BIGINT);
        JDBC_TYPE_MAPPING.put(long.class, JdbcType.BIGINT);

        JDBC_TYPE_MAPPING.put(Float.class, JdbcType.FLOAT);
        JDBC_TYPE_MAPPING.put(float.class, JdbcType.FLOAT);

        JDBC_TYPE_MAPPING.put(Double.class, JdbcType.DOUBLE);
        JDBC_TYPE_MAPPING.put(double.class, JdbcType.DOUBLE);

        JDBC_TYPE_MAPPING.put(String.class, JdbcType.VARCHAR);
        JDBC_TYPE_MAPPING.put(Character.class, JdbcType.VARCHAR);
        JDBC_TYPE_MAPPING.put(char.class, JdbcType.VARCHAR);

        JDBC_TYPE_MAPPING.put(BigInteger.class, JdbcType.BIGINT);

        JDBC_TYPE_MAPPING.put(BigDecimal.class, JdbcType.DECIMAL);

        JDBC_TYPE_MAPPING.put(byte[].class, JdbcType.BLOB);
        JDBC_TYPE_MAPPING.put(Byte[].class, JdbcType.BLOB);

        JDBC_TYPE_MAPPING.put(Date.class, JdbcType.TIMESTAMP);
        JDBC_TYPE_MAPPING.put(java.sql.Date.class, JdbcType.DATE);
        JDBC_TYPE_MAPPING.put(LocalTime.class, JdbcType.TIME);
        JDBC_TYPE_MAPPING.put(LocalDate.class, JdbcType.TIMESTAMP);
        JDBC_TYPE_MAPPING.put(LocalDateTime.class, JdbcType.TIMESTAMP);

        EXCLUDE_IMPORTS.add("me.ningpp.mmegp.annotations.Generated");
        EXCLUDE_IMPORTS.add("me.ningpp.mmegp.annotations.GeneratedColumn");
        EXCLUDE_IMPORTS.add("org.apache.ibatis.type.JdbcType");
    }

    private MyBatisGeneratorUtil() {
    }

    public static JdbcType getJdbcTypeByClass(Class<?> clazz) {
        return JDBC_TYPE_MAPPING.get(clazz);
    }

    private static IntrospectedTable buildIntrospectedTable(Context context, 
            String modelPackageName, String mapperPackageName, ClassOrInterfaceDeclaration modelDeclaration) throws ClassNotFoundException {
        String tableName = JavaParserUtil.getTableValue(modelDeclaration.getAnnotationByClass(Generated.class));
        String fullName = modelDeclaration.getFullyQualifiedName().get();
        String domainObjectName = fullName.substring(fullName.lastIndexOf('.')+1, fullName.length());
        IntrospectedTable introspectedTable = ObjectFactory.createIntrospectedTableForValidation(context);
        FullyQualifiedTable table = new FullyQualifiedTable(null, null, tableName, domainObjectName, null, false, null, null, null, false, null, context);
        introspectedTable.setFullyQualifiedTable(table);
        
        introspectedTable.setContext(context);
        introspectedTable.setBaseRecordType(modelDeclaration.getFullyQualifiedName().get());
        TableConfiguration tableConfiguration = new TableConfiguration(context);
        tableConfiguration.setDomainObjectName(domainObjectName);
        introspectedTable.setTableConfiguration(tableConfiguration);
        introspectedTable.setExampleType(modelDeclaration.getFullyQualifiedName().get() + "Example");
        introspectedTable.setMyBatis3JavaMapperType(mapperPackageName + "." + domainObjectName + "Mapper");
        List<FieldDeclaration> fields = modelDeclaration.getFields();
        if (fields == null || fields.size() == 0) {
            return null;
        }
        for (FieldDeclaration fieldDeclaration : fields) {
            Pair<IntrospectedColumn, Boolean> pair = JavaParserUtil.buildColumn(modelDeclaration, fieldDeclaration, context);
            if (pair != null) {
                introspectedTable.addColumn(pair.getLeft());
                if (Boolean.TRUE.equals(pair.getRight())) {
                    introspectedTable.addPrimaryKeyColumn(pair.getLeft().getActualColumnName());
                }
                if (pair.getLeft().isIdentity()) {
                    tableConfiguration.setGeneratedKey(new GeneratedKey(pair.getLeft().getActualColumnName(), "JDBC", true, null));
                }
            }
        }
        if (introspectedTable.getAllColumns().isEmpty()) {
            return null;
        }
        introspectedTable.initialize();
        //必须在initialize方法之后，否则rules不起作用
        introspectedTable.setRules(new MmegpFlatModelRules(introspectedTable));
        introspectedTable.calculateGenerators(Collections.emptyList(), new NullProgressCallback());
        return introspectedTable;
    }

    public static IntrospectedTable buildIntrospectedTable(Context context, 
            String modelPackageName, String mapperPackageName, CompilationUnit compilationUnit) throws ClassNotFoundException {
        Optional<TypeDeclaration<?>> ptOptional = compilationUnit.getPrimaryType();
        TypeDeclaration<?> typeDeclaration;
        if (ptOptional.isPresent()) {
            typeDeclaration = ptOptional.get();
        } else if (compilationUnit.getTypes().size() > 0) {
            typeDeclaration = compilationUnit.getType(0);
        } else {
            typeDeclaration = null;
        }
        if (!(typeDeclaration instanceof ClassOrInterfaceDeclaration) 
                || !typeDeclaration.isClassOrInterfaceDeclaration()) {
            return null;
        }
        ClassOrInterfaceDeclaration modelDeclaration = (ClassOrInterfaceDeclaration) typeDeclaration;
        if (modelDeclaration.isInterface()) {
            return null;
        }
        if (! modelDeclaration.isPublic()) {
            return null;
        }
        return buildIntrospectedTable(context, modelPackageName, mapperPackageName, modelDeclaration);
    }

}
