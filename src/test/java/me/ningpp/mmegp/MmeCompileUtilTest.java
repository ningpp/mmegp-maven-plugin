package me.ningpp.mmegp;

import static org.junit.jupiter.api.Assertions.assertEquals;
import static org.junit.jupiter.api.Assertions.assertNotNull;

import java.io.IOException;
import java.util.Collections;
import java.util.List;

import org.junit.jupiter.api.Test;
import org.mybatis.generator.api.IntrospectedColumn;
import org.mybatis.generator.api.IntrospectedTable;
import org.mybatis.generator.config.CommentGeneratorConfiguration;
import org.mybatis.generator.config.Context;
import org.mybatis.generator.config.JavaClientGeneratorConfiguration;
import org.mybatis.generator.config.JavaModelGeneratorConfiguration;
import org.mybatis.generator.config.ModelType;
import org.mybatis.generator.config.PropertyRegistry;
import org.mybatis.generator.config.SqlMapGeneratorConfiguration;
import org.mybatis.generator.internal.NullProgressCallback;

/**
 * 
 * @author ningpp
 * @date 2021-09-29 14:54:21
 */
public class MmeCompileUtilTest {

    private static final String JAVA_SOURCE_FILE_CONTENT = "    package me.ningpp.mmegp.entity;\r\n"
            + "\r\n"
            + "    import me.ningpp.mmegp.annotations.Generated;\r\n"
            + "    import me.ningpp.mmegp.annotations.GeneratedColumn;\r\n"
            + "    import org.apache.ibatis.type.JdbcType;\r\n"
            + "\r\n"
            + "    @Generated(table = \"test_entity\")\r\n"
            + "    public class TestEntity {\r\n"
            + "        @GeneratedColumn(name = \"ID\", jdbcType = JdbcType.VARCHAR, id = true, blob = false, generatedValue = false)\r\n"
            + "        private String id;\r\n"
            + "\r\n"
            + "        @GeneratedColumn(name = \"IMAGE_DATA\", jdbcType = JdbcType.LONGVARBINARY, id = false, blob = true, generatedValue = false)\r\n"
            + "        private byte[] imageData;\r\n"
            + "\r\n"
            + "        @GeneratedColumn(name = \"IMAGE_DATA2\", jdbcType = JdbcType.LONGVARBINARY, id = false, blob = true, generatedValue = false)\r\n"
            + "        private Byte[] imageData;\r\n"
            + "\r\n"
            + "        public String getId() {\r\n"
            + "            return id;\r\n"
            + "        }\r\n"
            + "\r\n"
            + "        public void setId(String id) {\r\n"
            + "            this.id = id;\r\n"
            + "        }\r\n"
            + "\r\n"
            + "        public byte[] getImageData() {\r\n"
            + "            return imageData;\r\n"
            + "        }\r\n"
            + "\r\n"
            + "        public void setImageData(byte[] imageData) {\r\n"
            + "            this.imageData = imageData;\r\n"
            + "        }\r\n"
            + "\r\n"
            + "        public Byte[] getImageData2() {\r\n"
            + "            return imageData2;\r\n"
            + "        }\r\n"
            + "\r\n"
            + "        public void setImageData2(Byte[] imageData2) {\r\n"
            + "            this.imageData2 = imageData2;\r\n"
            + "        }\r\n"
            + "    }\r\n"
            + "";

    @Test
    void buildIntrospectedTableTest() throws ClassNotFoundException, IOException, InterruptedException {
        String targetProject = System.getProperty("java.dir");
        String javaClientGeneratorConfigurationType = "XMLMAPPER";
        String modelPackageName = "me.ningpp.mmegp.entity";
        String mapperPackageName = "me.ningpp.mmegp.mapper";
        
        Context context = new Context(ModelType.FLAT);

        CommentGeneratorConfiguration commentGeneratorConfiguration = new CommentGeneratorConfiguration();
        commentGeneratorConfiguration.addProperty(PropertyRegistry.COMMENT_GENERATOR_SUPPRESS_DATE, Boolean.FALSE.toString());
        commentGeneratorConfiguration.addProperty(PropertyRegistry.COMMENT_GENERATOR_SUPPRESS_ALL_COMMENTS, Boolean.TRUE.toString());
        commentGeneratorConfiguration.addProperty(PropertyRegistry.COMMENT_GENERATOR_ADD_REMARK_COMMENTS, Boolean.FALSE.toString());
        commentGeneratorConfiguration.addProperty(PropertyRegistry.COMMENT_GENERATOR_DATE_FORMAT, "");
        context.setCommentGeneratorConfiguration(commentGeneratorConfiguration);

        SqlMapGeneratorConfiguration sqlMapGeneratorConfiguration = new SqlMapGeneratorConfiguration();
        sqlMapGeneratorConfiguration.setTargetProject(targetProject);
        sqlMapGeneratorConfiguration.setTargetPackage(mapperPackageName);
        context.setSqlMapGeneratorConfiguration(sqlMapGeneratorConfiguration);

        JavaClientGeneratorConfiguration javaClientGeneratorCfg = new JavaClientGeneratorConfiguration();
        javaClientGeneratorCfg.setTargetProject(targetProject);
        javaClientGeneratorCfg.setConfigurationType(javaClientGeneratorConfigurationType);
        javaClientGeneratorCfg.setTargetPackage(mapperPackageName);
        context.setJavaClientGeneratorConfiguration(javaClientGeneratorCfg);

        JavaModelGeneratorConfiguration jmgConfig = new JavaModelGeneratorConfiguration();
        jmgConfig.setTargetProject(targetProject);
        jmgConfig.setTargetPackage(modelPackageName);
        jmgConfig.addProperty(PropertyRegistry.MODEL_GENERATOR_EXAMPLE_PACKAGE, modelPackageName);
        jmgConfig.addProperty(PropertyRegistry.COMMENT_GENERATOR_SUPPRESS_ALL_COMMENTS, Boolean.TRUE.toString());
        context.setJavaModelGeneratorConfiguration(jmgConfig);

        //为了初始化pluginAggregator
        context.generateFiles(new NullProgressCallback(), Collections.emptyList(), Collections.emptyList(), Collections.emptyList(), Collections.emptyList());

        IntrospectedTable introspectedTable = MmeCompileUtil.buildIntrospectedTable(context, 
                modelPackageName, mapperPackageName, JAVA_SOURCE_FILE_CONTENT);
        assertNotNull(introspectedTable);
        List<IntrospectedColumn> columns = introspectedTable.getAllColumns();
        assertEquals(3, columns.size());
        assertEquals("ID", columns.get(0).getActualColumnName());
        assertEquals("IMAGE_DATA", columns.get(1).getActualColumnName());
        assertEquals("IMAGE_DATA2", columns.get(2).getActualColumnName());
    }

}
